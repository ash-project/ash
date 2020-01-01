defmodule Ash.Authorization.Checker do
  alias Ash.Authorization.Request
  alias Ash.Actions.SideLoad

  # TODO: strict_check can't do things with dependencies. Meaning,
  # we need to run strict check for things with dependencies in the
  # second phase. So we should prioritize things in this way:
  # 1.) Things who's dependencies unlock strict checks
  # 2.) things who's strict checks were never run
  # 3.) Generate the changeset for those
  # 3.5) probably make it invalid to have an auth request with a changeset function
  #      but no dependencies.
  # 4.) run strict checks

  def strict_check(user, request, facts, strict_access?) do
    if Request.can_strict_check?(request) do
      new_facts =
        request.authorization_steps
        |> Enum.reduce(facts, fn {_step, clause}, facts ->
          case Map.fetch(facts, {request.relationship, clause}) do
            {:ok, _boolean_result} ->
              facts

            :error ->
              case do_strict_check(clause, user, request, strict_access?) do
                :unknown ->
                  facts

                :unknowable ->
                  Map.put(facts, clause, :unknowable)

                :irrelevant ->
                  Map.put(facts, clause, :irrelevant)

                boolean ->
                  Map.put(facts, clause, boolean)
              end
          end
        end)

      {Map.put(request, :strict_check_completed?, true), new_facts}
    else
      {request, facts}
    end
  end

  def run_checks(scenarios, user, requests, facts, state, strict_access?) do
    all_checkable_clauses = all_checkable_clauses_from_scenarios(scenarios, facts)

    case clauses_checkable_without_fetching_data(all_checkable_clauses, requests, state) do
      {[], []} ->
        :all_scenarios_known

      {[], _clauses_requiring_fetch} ->
        case fetch_requests(requests, state, strict_access?) do
          {:ok, {new_requests, new_state}} ->
            run_checks(scenarios, user, new_requests, facts, new_state, strict_access?)

          :all_scenarios_known ->
            :all_scenarios_known

          {:error, error} ->
            {:error, error}
        end

      {clauses, _} ->
        # TODO: We could limit/smartly choose the checks that we prepare and run here as an optimization
        case prepare_checks(clauses, requests, state) do
          {:ok, new_state} ->
            case do_run_checks(clauses, user, requests, facts, new_state, strict_access?) do
              {:ok, new_facts, new_state} -> {:ok, requests, new_facts, new_state}
              {:error, error} -> {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end
    end
  end

  # TODO: We could be smart here, and likely fetch multiple requests at a time
  defp fetch_requests(requests, state, strict_access?) do
    {fetchable_requests, other_requests} =
      Enum.split_with(requests, fn request ->
        bypass_strict? =
          if strict_access? do
            request.bypass_strict_access?
          else
            true
          end

        bypass_strict? && !Request.fetched?(state, request) &&
          Request.dependencies_met?(state, request)
      end)

    fetchable_requests
    |> Enum.filter(fn request ->
      Request.dependencies_met?(state, request) && request.strict_check_completed?
    end)
    |> Enum.map(fn request ->
      Request.fetch_changeset(state, request)
    end)
    |> Enum.sort_by(fn request ->
      # Requests that bypass strict access should generally perform well
      # as they would generally be more efficient checks
      {Enum.count(request.relationship), not request.bypass_strict_access?, request.relationship}
    end)
    |> case do
      [request | _] = requests ->
        case Request.fetch(state, request) do
          {:ok, new_state} ->
            {:ok, {requests ++ other_requests, new_state}}

          :error ->
            {:ok, {requests ++ other_requests, state}}
        end

      _ ->
        :all_scenarios_known
    end
  end

  defp do_run_checks(clauses, user, requests, facts, state, strict_access?) do
    Enum.reduce_while(clauses, {:ok, facts, state}, fn clause, {:ok, facts, state} ->
      request =
        requests
        # This puts all requests with `bypass_strict_access?` in the front
        # because if we can we want to find one of those first for the check below
        |> Enum.sort_by(fn request ->
          not request.bypass_strict_access?
        end)
        |> Enum.find(fn request ->
          Request.contains_clause?(request, clause)
        end) || raise "Internal assumption failed"

      {:ok, request_state} = Request.fetch_request_state(state, request)
      request_state = List.wrap(request_state)

      check_module = clause.check_module
      check_opts = clause.check_opts

      cond do
        request_state == [] and strict_access? and !request.bypass_strict_access? ->
          {:cont, {:ok, Map.put(facts, clause, :unknowable), state}}

        request_state == [] ->
          {:cont, {:ok, Map.put(facts, clause, :irrelevant), state}}

        true ->
          # TODO: Determine whether or not checks need the ability to generate additional state.
          # If they do, we need to store that additional check state in `state` and pass it in here
          case check_module.check(user, request_state, %{}, check_opts) do
            {:error, error} ->
              {:halt, {:error, error}}

            {:ok, check_result} ->
              {:cont,
               {:ok, add_check_results_to_facts(clause, check_result, request_state, facts),
                state}}
          end
      end
    end)
  end

  defp clauses_checkable_without_fetching_data([], _, _), do: {[], []}

  defp clauses_checkable_without_fetching_data(clauses, requests, state) do
    Enum.split_with(clauses, fn clause ->
      Enum.any?(requests, fn request ->
        Request.fetched?(state, request) && Request.contains_clause?(request, clause) &&
          Request.dependencies_met?(state, request)
      end)
    end)
  end

  defp all_checkable_clauses_from_scenarios(scenarios, facts) do
    scenarios
    |> Enum.flat_map(fn scenario ->
      scenario
      |> Map.drop([true, false])
      |> Enum.map(&elem(&1, 0))
    end)
    |> Enum.reject(fn clause ->
      Map.has_key?(facts, clause)
    end)
  end

  # Check returning `{:ok, true}` means all records are authorized
  # while `{:ok, false}` means all records are not
  defp add_check_results_to_facts(clause, boolean, _data, facts) when is_boolean(boolean) do
    Map.put(facts, clause, boolean)
  end

  defp add_check_results_to_facts(clause, [], _data, facts), do: Map.put(facts, clause, false)

  defp add_check_results_to_facts(clause, [%resource{} | _] = records, data, facts) do
    pkey = Ash.primary_key(resource)
    record_pkeys = Enum.map(records, &Map.take(&1, pkey))

    case Enum.split_with(data, fn record ->
           Map.take(record, pkey) in record_pkeys
         end) do
      {[], _} ->
        Map.put(facts, clause, false)

      {_, []} ->
        Map.put(facts, clause, true)

      {true_data, false_data} ->
        facts = set_records_to(true_data, facts, clause, true, pkey)

        set_records_to(false_data, facts, clause, false, pkey)
    end
  end

  defp set_records_to(data, facts, clause, value, pkey) do
    Enum.reduce(data, facts, fn record, facts ->
      pkey_clause = %{clause | pkey: Map.take(record, pkey)}

      facts
      |> Map.put(pkey_clause, value)
    end)
  end

  defp prepare_checks(checks, requests, state) do
    checks
    |> group_fetched_checks_by_request(requests, state)
    |> Enum.reduce_while({:ok, state}, fn {request, checks}, {:ok, state} ->
      {:ok, data} = Request.fetch_request_state(state, request)

      case get_preparation(checks) do
        {:ok, preparations} ->
          case run_preparations(request, data, preparations) do
            {:ok, new_data} ->
              {:cont, {:ok, Request.put_request_state(state, request, new_data)}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp run_preparations(request, data, preparations) do
    Enum.reduce_while(preparations, {:ok, data}, fn {name, value}, {:ok, data} ->
      case run_preparation(request, data, name, value) do
        {:ok, new_data} -> {:cont, {:ok, new_data}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp run_preparation(_, [], :side_load, _), do: {:ok, []}
  defp run_preparation(_, nil, :side_load, _), do: {:ok, nil}

  defp run_preparation(request, data, :side_load, side_load) do
    SideLoad.side_load(request.resource, data, side_load, request.api)
  end

  defp run_preparation(_, _, preparation, _), do: {:error, "Unknown preparation #{preparation}"}

  defp get_preparation(checks) do
    Enum.reduce_while(checks, {:ok, %{}}, fn check, {:ok, preparations} ->
      case check.check_module.prepare(check.check_opts) do
        [] ->
          {:cont, {:ok, preparations}}

        new_preparations ->
          case do_add_preparations(new_preparations, preparations) do
            {:ok, combined_preparations} -> {:cont, {:ok, combined_preparations}}
            {:error, error} -> {:halt, {:error, error}}
          end
      end
    end)
  end

  defp do_add_preparations(new_preparations, preparations) do
    Enum.reduce_while(new_preparations, {:ok, preparations}, fn {name, value},
                                                                {:ok, preparations} ->
      case add_preparation(name, value, preparations) do
        {:ok, preparations} -> {:cont, {:ok, preparations}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp add_preparation(:side_load, side_load, preparations) do
    {:ok, Map.update(preparations, :side_load, side_load, &SideLoad.merge(&1, side_load))}
  end

  defp add_preparation(preparation, _, _) do
    {:error, "Unkown preparation #{preparation}"}
  end

  defp group_fetched_checks_by_request(clauses, requests, state) do
    requests =
      Enum.sort_by(requests, fn request ->
        # Requests that bypass strict access should generally perform well
        # as they would generally be more efficient checks
        {Enum.count(request.relationship), not request.bypass_strict_access?,
         request.relationship}
      end)

    Enum.group_by(clauses, fn clause ->
      Enum.find(requests, fn request ->
        Request.fetched?(state, request) && Request.contains_clause?(request, clause)
      end) || raise "Assumption failed"
    end)
  end

  defp do_strict_check(%{check_module: module, check_opts: opts}, user, request, strict_access?) do
    case module.strict_check(user, request, opts) do
      {:ok, boolean} when is_boolean(boolean) ->
        boolean

      {:ok, :irrelevant} ->
        :irrelevant

      {:ok, :unknown} ->
        cond do
          strict_access? && not request.bypass_strict_access? ->
            # This means "we needed a fact that we have no way of getting"
            # Because the fact was needed in the `strict_check` step
            :unknowable

          Ash.Authorization.Check.defines_check?(module) ->
            :unknown

          true ->
            :unknowable
        end
    end
  end
end
