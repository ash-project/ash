defmodule Ash.Engine do
  require Logger
  alias Ash.Engine.Request
  # TODO: Add ability to configure "resolver error behavior"
  # graphql will want to continue on failures, but the
  # code interface/JSON API will want to bail on the first error

  alias Ash.Authorization.{Checker, Clause, SatSolver}

  defstruct [
    :api,
    :requests,
    :user,
    :log_transitions?,
    :failure_mode,
    errors: %{},
    completed_preparations: %{},
    data: %{},
    state: :init,
    facts: %{
      true: true,
      false: false
    },
    scenarios: []
  ]

  @states [
    :init,
    :check,
    :generate_scenarios,
    :reality_check,
    :resolve_some,
    :resolve_complete,
    :complete
  ]

  def run(requests, api, opts \\ []) do
    requests
    |> new(api, opts)
    |> loop_until_complete()
  end

  defp loop_until_complete(engine) do
    case next(engine) do
      %{state: :complete} = new_engine ->
        if all_resolved_or_unnecessary?(new_engine.requests) do
          new_engine
        else
          add_error(engine, [:__engine__], "Completed without all data resolved.")
        end

      new_engine when new_engine == engine ->
        transition(new_engine, :complete, %{
          errors: {:__engine__, "State machine stuck in infinite loop"}
        })

      new_engine ->
        loop_until_complete(new_engine)
    end
  end

  defp all_resolved_or_unnecessary?(requests) do
    requests
    |> Enum.filter(& &1.resolve_when_fetch_only?)
    |> Enum.all?(&Request.data_resolved?/1)
  end

  defp next(%{failure_mode: :complete, errors: errors} = engine) when errors != %{} do
    transition(
      engine,
      :complete,
      %{
        errors:
          {:__engine__, "Got an error, failure mode set to complete, failing entire request."}
      }
    )
  end

  defp next(%{state: :init} = engine) do
    engine.requests
    |> Enum.reduce(engine, &replace_request(&2, &1))
    |> transition(:check)
  end

  defp next(%{state: :check} = engine) do
    new_engine =
      case strict_check(engine) do
        ^engine ->
          check(engine)

        new_engine ->
          new_engine
      end

    transition(new_engine, :generate_scenarios)
  end

  defp next(%{state: :generate_scenarios} = engine) do
    generate_scenarios(engine)
  end

  defp next(%{state: :reality_check} = engine) do
    reality_check(engine)
  end

  defp next(%{state: :resolve_some} = engine) do
    # TODO: We should probably find requests that can be fetched in parallel
    # and fetch them asynchronously (if their data layer allows it)
    case resolvable_requests(engine) do
      [request | _rest] ->
        # TODO: run any preparations on the data here, and then store what preparations have been run on what data so
        # we don't run them again.

        engine
        |> resolve_data(request)
        |> transition(:check)

      [] ->
        transition(engine, :complete, %{message: "No requests to resolve"})
    end
  end

  defp next(%{state: :resolve_complete} = engine) do
    case Enum.find(engine.requests, &resolve_for_resolve_complete?(&1, engine)) do
      nil ->
        transition(engine, :complete, %{message: "No remaining requests that must be resolved"})

      request ->
        engine
        |> resolve_data(request)
        |> remain(%{message: "Resolved #{request.name}"})
    end
  end

  defp resolve_for_resolve_complete?(request, engine) do
    if Request.data_resolved?(request) do
      false
    else
      case Request.all_dependencies_met?(request, engine.data) do
        {true, _must_resolve} ->
          request.resolve_when_fetch_only? || is_hard_depended_on?(request, engine.requests)

        false ->
          false
      end
    end
  end

  defp is_hard_depended_on?(request, all_requests) do
    remaining_requests = all_requests -- [request]

    all_requests
    |> Enum.reject(& &1.error?)
    |> Enum.reject(&Request.data_resolved?/1)
    |> Enum.filter(&Request.depends_on?(&1, request))
    |> Enum.any?(fn other_request ->
      other_request.resolve_when_fetch_only? ||
        is_hard_depended_on?(other_request, remaining_requests)
    end)
  end

  defp prepare(_, %{prepared?: true} = request), do: {:ok, request}

  defp prepare(engine, request) do
    # Right now the only preparation is a side_load
    side_loads =
      Enum.reduce(request.rules, [], fn {_, clause}, preloads ->
        clause.check_opts
        |> clause.check_module.prepare()
        |> Enum.reduce(preloads, fn {:side_load, path}, preloads ->
          Ash.Actions.SideLoad.merge(preloads, path)
        end)
      end)

    case Ash.Actions.SideLoad.side_load(
           engine.api,
           request.resource,
           request.data,
           side_loads,
           request.filter
         ) do
      {:ok, new_request_data} ->
        new_request = %{request | data: new_request_data, prepared?: true}
        {:ok, new_request}

      {:error, error} ->
        {:error, request.path ++ [:__prepare__], error, engine}
    end
  end

  defp replace_request(engine, new_request, replace_data? \\ true) do
    new_requests =
      Enum.map(engine.requests, fn request ->
        if request.id == new_request.id do
          new_request
        else
          request
        end
      end)

    if replace_data? && new_request.write_to_data? do
      new_engine_data = Request.put_request(engine.data, new_request)
      %{engine | data: new_engine_data, requests: new_requests}
    else
      %{engine | requests: new_requests}
    end
  end

  defp resolvable_requests(engine) do
    Enum.filter(engine.requests, fn request ->
      !request.error? && not request.strict_access? &&
        match?(%Request.UnresolvedField{}, request.data) &&
        match?({true, _}, Request.all_dependencies_met?(request, engine.data)) &&
        maybe_passes_strict_check_in_isolation?(request, engine)
    end)
  end

  defp maybe_passes_strict_check_in_isolation?(request, engine) do
    rules_with_data =
      if Request.data_resolved?(request) do
        request.data
        |> List.wrap()
        |> Enum.map(fn item ->
          {request.rules, get_pkeys(item, engine.api)}
        end)
      else
        [request.rules]
      end

    case SatSolver.solve(rules_with_data, engine.facts) do
      {:ok, _scenarios} ->
        true

      {:error, :unsatisfiable} ->
        false
    end
  end

  defp resolve_data(engine, request) do
    with {:ok, requirements_resolved} <- resolve_required_paths(engine, request),
         {:ok, resolved_request} <- Request.resolve_data(requirements_resolved.data, request),
         {:ok, prepared_request} <- prepare(engine, resolved_request) do
      replace_request(engine, prepared_request)
    else
      {:error, path, message, engine} ->
        add_error(engine, path, message)

      {:error, error} ->
        new_request = %{request | error?: true}

        engine
        |> replace_request(new_request)
        |> add_error(request.path ++ [:data], error)
    end
  end

  defp resolve_required_paths(engine, request) do
    case Request.all_dependencies_met?(request, engine.data) do
      false ->
        raise "Unreachable case"

      {true, dependency_paths} ->
        do_resolve_required_paths(dependency_paths, engine, request)
    end
  end

  defp do_resolve_required_paths(dependency_paths, engine, request) do
    resolution_result =
      dependency_paths
      |> Enum.sort_by(&Enum.count/1)
      |> Enum.reduce_while({:ok, engine, []}, fn path, {:ok, engine, skipped} ->
        case resolve_by_path(path, engine.data, engine.data) do
          {data, requests} ->
            {:cont,
             {:ok, Enum.reduce(requests, %{engine | data: data}, &replace_request(&2, &1, false)),
              skipped}}

          {:unmet_dependencies, new_data, new_requests} ->
            new_engine =
              Enum.reduce(
                new_requests,
                %{engine | data: new_data},
                &replace_request(&2, &1, false)
              )

            {:cont, {:ok, new_engine, skipped ++ [path]}}

          {:error, new_data, new_requests, path, error} ->
            new_engine =
              engine
              |> Map.put(:data, new_data)
              |> replace_request(%{request | error?: true})
              |> add_error(request.path, error)

            {:halt,
             {:error, path, error,
              Enum.reduce(new_requests, new_engine, &replace_request(&2, &1, false))}}
        end
      end)

    case resolution_result do
      {:ok, engine, ^dependency_paths} when dependency_paths != [] ->
        [first | rest] = dependency_paths

        {:error, first, "Codependent requests.",
         Enum.reduce(rest, engine, &add_error(&2, &1, "Codependent requests."))}

      {:ok, engine, []} ->
        {:ok, engine}

      {:ok, engine, skipped} ->
        do_resolve_required_paths(skipped, engine, request)
    end
  end

  defp resolve_by_path(path, current_data, all_data, requests \\ [], path_prefix \\ [])

  defp resolve_by_path([head | tail], current_data, all_data, requests, path_prefix)
       when is_map(current_data) do
    case Map.fetch(current_data, head) do
      {:ok, %Request{} = request} ->
        case resolve_by_path(tail, request, all_data, requests, [head | path_prefix]) do
          {:error, new_request, new_requests, error_path, message} ->
            {:error, Map.put(current_data, request, new_request),
             [%{new_request | error?: true} | new_requests], error_path, message}

          {new_request, new_requests} ->
            {Map.put(current_data, head, new_request), [new_request | new_requests]}

          {:unmet_dependencies, new_request, new_requests} ->
            {:unmet_dependencies, Map.put(current_data, request, new_request),
             [new_request | new_requests]}
        end

      {:ok, %Request.UnresolvedField{}} when tail != [] ->
        {:error, current_data, requests, Enum.reverse(path_prefix) ++ [head],
         "Unresolved field while resolving path"}

      {:ok, value} ->
        case resolve_by_path(tail, value, all_data, requests, [head | path_prefix]) do
          {:error, nested_data, new_requests, error_path, message} ->
            {:error, Map.put(current_data, value, nested_data), new_requests, error_path, message}

          {new_value, new_requests} ->
            {Map.put(current_data, head, new_value), new_requests}

          {:unmet_dependencies, new_value, new_requests} ->
            {:unmet_dependencies, Map.put(current_data, head, new_value), new_requests}
        end

      nil ->
        {:error, current_data, requests, Enum.reverse(path_prefix) ++ [head],
         "Missing field while resolving path"}
    end
  end

  defp resolve_by_path([], value, all_data, requests, path_prefix) do
    case value do
      %Request.UnresolvedField{} = unresolved ->
        case Request.dependencies_met?(all_data, unresolved.depends_on) do
          {true, []} ->
            case Request.resolve_field(all_data, unresolved) do
              {:ok, value} -> {value, requests}
              {:error, error} -> {:error, value, requests, Enum.reverse(path_prefix), error}
            end

          {true, _needs} ->
            {:unmet_dependencies, unresolved, requests}

          false ->
            {:error, value, requests, Enum.reverse(path_prefix),
             "Unmet dependencies while resolving path"}
        end

      other ->
        {other, requests}
    end
  end

  defp resolve_by_path(path, current_data, _all_data, requests, path_prefix) do
    {:error, current_data, requests, Enum.reverse(path_prefix) ++ path,
     "Invalid data while resolving path."}
  end

  defp reality_check(engine) do
    case find_real_scenario(engine.scenarios, engine.facts) do
      nil ->
        transition(engine, :resolve_some, %{message: "No scenario was reality"})

      scenario ->
        scenario = Map.drop(scenario, [true, false])

        transition(engine, :resolve_complete, %{
          message: "Scenario was reality: #{inspect(scenario)}"
        })
    end
  end

  defp find_real_scenario(scenarios, facts) do
    Enum.find_value(scenarios, fn scenario ->
      if scenario_is_reality(scenario, facts) == :reality do
        scenario
      else
        false
      end
    end)
  end

  defp scenario_is_reality(scenario, facts) do
    scenario
    |> Map.drop([true, false])
    |> Enum.reduce_while(:reality, fn {fact, requirement}, status ->
      case Map.fetch(facts, fact) do
        {:ok, value} ->
          cond do
            value == requirement ->
              {:cont, status}

            value == :irrelevant ->
              {:cont, status}

            value == :unknowable ->
              {:halt, :not_reality}

            true ->
              {:halt, :not_reality}
          end

        :error ->
          {:cont, :maybe}
      end
    end)
  end

  defp generate_scenarios(engine) do
    rules_with_data =
      Enum.flat_map(engine.requests, fn request ->
        if Request.data_resolved?(request) do
          request.data
          |> List.wrap()
          |> Enum.map(fn item ->
            {request.rules, get_pkeys(item, engine.api)}
          end)
        else
          [request.rules]
        end
      end)

    case SatSolver.solve(rules_with_data, engine.facts) do
      {:ok, scenarios} ->
        transition(engine, :reality_check, %{scenarios: scenarios})

      {:error, :unsatisfiable} ->
        error =
          Ash.Error.Forbidden.exception(
            requests: engine.requests,
            facts: engine.facts,
            state: engine.data,
            reason: "No scenario leads to authorization"
          )

        transition(engine, :complete, %{errors: {:__engine__, error}})
    end
  end

  defp get_pkeys(%resource{} = item, api) do
    pkey_filter =
      item
      |> Map.take(Ash.primary_key(resource))
      |> Map.to_list()

    Ash.Filter.parse(resource, pkey_filter, api)
  end

  defp check(engine) do
    case checkable_request(engine) do
      {:ok, request} ->
        run_checks(engine, request)

      _ ->
        engine
    end
  end

  defp run_checks(engine, request) do
    request.rules
    |> Enum.filter(fn {_kind, clause}, engine ->
      case Clause.find(engine.facts, clause) do
        {:ok, :unknown} -> true
        {:ok, :unknowable} -> true
        {:ok, _} -> false
        :error -> true
      end
    end)
    |> Enum.reduce_while(engine, fn {_kind, clause}, engine ->
      # TODO: remove the third argument below/just pass in the request
      case Checker.run_checks(engine, request, clause) do
        {:error, error} ->
          engine =
            engine
            |> add_error(request.path, error)
            |> replace_request(engine, %{request | error?: true, check_complete?: true})

          {:halt, engine}

        {:ok, new_engine} ->
          engine = replace_request(new_engine, %{request | check_complete?: true})

          {:cont, engine}
      end
    end)
  end

  defp checkable_request(engine) do
    engine.requests
    |> Enum.filter(& &1.strict_check_completed?)
    |> Enum.filter(&Request.data_resolved?/1)
    |> Enum.reject(& &1.check_complete?/1)
    |> Enum.reject(& &1.error?)
    |> case do
      [request | _] ->
        {:ok, request}

      _ ->
        :error
    end
  end

  defp strict_check(engine) do
    {requests, facts} =
      Enum.reduce(engine.requests, {[], engine.facts}, fn request, {requests, facts} ->
        {new_request, new_facts} = Checker.strict_check(engine.user, request, facts)

        {[new_request | requests], new_facts}
      end)

    transition(engine, :generate_scenarios, %{requests: Enum.reverse(requests), facts: facts})
  end

  defp new(request, api, opts) when not is_list(request), do: new([request], api, opts)

  defp new(requests, api, opts) do
    # TODO: We should put any pre-resolved data into state
    requests =
      if opts[:fetch_only?] do
        Enum.map(requests, &Request.authorize_always/1)
      else
        requests
      end

    engine = %__MODULE__{
      requests: requests,
      user: opts[:user],
      api: api,
      failure_mode: opts[:failure_mode] || :complete,
      log_transitions?: Keyword.get(opts, :log_transitions, true)
    }

    if engine.log_transitions? do
      Logger.debug(
        "Initializing engine with requests: #{
          Enum.map_join(requests, ", ", &(to_string(&1.resource) <> ": " <> &1.name))
        }"
      )
    end

    case Enum.find(requests, &Enum.empty?(&1.rules)) do
      nil ->
        engine

      request ->
        exception = Ash.Error.Forbidden.exception(no_steps_configured: request)

        if opts[:log_final_report?] do
          Logger.info(Ash.Error.Forbidden.report_text(exception))
        end

        transition(engine, :complete, %{errors: {:__engine__, exception}})
    end
  end

  defp format_args(%{message: message} = args) do
    case clean_args(args) do
      "" ->
        " | #{message}"

      output ->
        " | #{message}#{output}"
    end
  end

  defp format_args(args) do
    clean_args(args)
  end

  defp clean_args(args) do
    args
    |> case do
      %{scenarios: scenarios} = args ->
        Map.put(args, :scenarios, "...#{Enum.count(scenarios)} scenarios")

      other ->
        other
    end
    |> Map.delete(:message)
    |> case do
      args when args == %{} ->
        ""

      args ->
        " | " <> inspect(args)
    end
  end

  defp remain(engine, args) do
    if engine.log_transitions? do
      Logger.debug("Remaining in #{engine.state}#{format_args(args)}")
    end

    engine
    |> handle_args(args)
  end

  defp transition(engine, state, args \\ %{}) do
    if engine.log_transitions? do
      Logger.debug("Moving from #{engine.state} to #{state}#{format_args(args)}")
    end

    engine
    |> handle_args(args)
    |> do_transition(state, args)
  end

  defp do_transition(engine, state, _args) when state not in @states do
    do_transition(engine, :complete, %{errors: {:__engine__, "No such state #{state}"}})
  end

  defp do_transition(engine, state, _args) do
    %{engine | state: state}
  end

  defp handle_args(engine, args) do
    engine
    |> handle_request_updates(args)
    |> handle_scenarios_updates(args)
    |> handle_facts_updates(args)
    |> handle_errors(args)
  end

  defp handle_scenarios_updates(engine, %{scenarios: scenarios}) do
    %{engine | scenarios: scenarios}
  end

  defp handle_scenarios_updates(engine, _), do: engine

  defp handle_facts_updates(engine, %{facts: facts}) do
    %{engine | facts: facts}
  end

  defp handle_facts_updates(engine, _), do: engine

  defp handle_request_updates(engine, %{requests: requests}) do
    %{engine | requests: requests}
  end

  defp handle_request_updates(engine, _), do: engine

  defp handle_errors(engine, %{errors: errors}) when is_map(errors) do
    handle_errors(engine, %{errors: Map.to_list(errors)})
  end

  defp handle_errors(engine, %{errors: error}) when not is_list(error) do
    handle_errors(engine, %{errors: List.wrap(error)})
  end

  defp handle_errors(engine, %{errors: errors}) when errors != [] do
    Enum.reduce(errors, engine, fn {path, error}, engine ->
      add_error(engine, path, error)
    end)
  end

  defp handle_errors(engine, _), do: engine

  defp put_nested_error(map, [key], error) do
    case map do
      value when is_map(value) ->
        Map.update(value, key, %{errors: [error]}, fn nested_value ->
          if is_map(nested_value) do
            Map.update(nested_value, :errors, [error], fn errors -> [error | errors] end)
          else
            %{errors: [value] ++ List.wrap(error)}
          end
        end)

      value ->
        %{key => %{errors: [value] ++ List.wrap(error)}}
    end
  end

  defp put_nested_error(map, [key | rest], error) do
    map
    |> Map.put_new(key, %{})
    |> Map.update!(key, &put_nested_error(&1, rest, error))
  end

  defp add_error(engine, path, error) do
    %{engine | errors: put_nested_error(engine.errors, List.wrap(path), error)}
  end
end
