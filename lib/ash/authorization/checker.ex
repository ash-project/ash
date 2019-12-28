defmodule Ash.Authorization.Checker do
  alias Ash.Authorization.Clause
  alias Ash.Authorization.Request

  def strict_check(user, request, facts, strict_access?) do
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

            boolean ->
              Map.put(facts, clause, boolean)
          end
      end
    end)
  end

  # TODO: Now, auth requests have a `fetcher`. We need to do the following:
  # Going in *descending order of relationship path length*, take each request
  # If it is `bypass_strict_access?`, fetch its data if necessary, and rerun auth.`
  # Store that data in state, so we can see that we've already fetched it later.
  # If not, move on and do the same for the rest of the relationships
  # If there aree no facts left to check for bypass_strict_checks, and `strict_access?` (the overall flag)
  # is true, then we return a forbidden error.
  # If it is not, we do the same process as above where we go in descending order of path length
  # except we run all fetchers, regardless of `bypass_strict_access?`, checking auth in between each one,
  # and iteratively fetching facts, that kind of thing.
  #
  #
  # In retrospect, this could perhaps be done by calling `run_checks` repeatedly, until it returns `{:require, blah}`
  # and if we are in strict mode, then we bail. It will run fetchers for anything with `bypass_strict_access?` if
  # it needs/wants to. THis is a better solution.`

  def run_checks(scenarios, user, requests, facts, state, strict_access?) do
    all_checkable_clauses = all_checkable_clauses_from_scenarios(scenarios, facts)

    case clauses_checkable_without_fetching_data(all_checkable_clauses, requests, state) do
      {[], []} ->
        :all_scenarios_known

      {[], _clauses_requiring_fetch} ->
        case fetch_requests(requests, state, strict_access?) do
          {:ok, new_state} ->
            run_checks(scenarios, user, requests, facts, new_state, strict_access?)

          :all_scenarios_known ->
            :all_scenarios_known

          {:error, error} ->
            {:error, error}
        end

      {clauses, _} ->
        # TODO: We could limit/smartly choose the checks that we prepare and run here as an optimization
        case prepare_checks(clauses, state) do
          {:ok, new_state} ->
            do_run_checks(clauses, user, requests, facts, new_state, strict_access?)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  # TODO: We could be smart here, and maybe fetch multiple requests
  defp fetch_requests(requests, state, strict_access?) do
    unfetched_requests =
      Enum.reject(requests, fn request ->
        Request.fetched?(state, request)
      end)

    requests_without_strict_access =
      if strict_access? do
        Enum.filter(unfetched_requests, fn request ->
          request.bypass_strict_access?
        end)
      else
        unfetched_requests
      end

    # We want to do these requests first regardless,
    # as they would generally be more efficient checks
    requests_without_strict_access
    |> Enum.sort_by(fn request ->
      {Enum.count(request.relationship), not request.bypass_strict_access?, request.relationship}
    end)
    |> Enum.at(0)
    |> case do
      nil ->
        :all_scenarios_known

      request ->
        case request.fetcher.() do
          {:ok, value} ->
            {:ok, Request.put_request_state(state, request, value)}

          {:error, error} ->
            {:error, error}
        end
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
          {:ok, Map.put(facts, clause, :unknowable), state}

        request_state == [] ->
          {:ok, Map.put(facts, clause, :irrelevant), state}

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
        Request.fetched?(state, request) && Request.contains_clause?(request, clause)
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
        Map.put(facts, clause, false)

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

  defp prepare_checks(clauses, state) do
    Enum.reduce_while(clauses, {:ok, state}, fn
      %{relationship: [], check_module: mod, check_opts: opts}, {:ok, state} ->
        case mod.prepare(opts) do
          [] -> {:cont, {:ok, state}}
          _ -> {:halt, {:error, "No preparations supported yet!"}}
        end

      _check, {_checks, _state} ->
        {:halt, {:error, "Can't prepare/handle checks with a relationship"}}
    end)
  end

  defp do_strict_check(%{check_module: module, check_opts: opts}, user, request, strict_access?) do
    case module.strict_check(user, request, opts) do
      {:ok, boolean} when is_boolean(boolean) ->
        boolean

      {:ok, :unknown} ->
        cond do
          strict_access? ->
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
