defmodule Ash.Engine2 do
  require Logger
  alias Ash.Engine2.Request
  # TODO: Add ability to configure "resolver error behavior"
  # graphql will want to continue on failures, but the
  # code interface/JSON API will want to bail on the first error

  alias Ash.Authorization.{Report, SatSolver}

  defstruct [
    :api,
    :requests,
    :user,
    :log_transitions?,
    :failure_mode,
    errors: %{},
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
    :resolve_fields,
    :strict_check,
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
        new_engine

      new_engine when new_engine == engine ->
        transition(new_engine, :complete, %{
          errors: {:__engine__, "State machine stuck in infinite loop"}
        })

      new_engine ->
        loop_until_complete(new_engine)
    end
  end

  defp next(%{failure_mode: :complete, errors: errors} = engine) when errors != %{} do
    transition(engine, :complete)
  end

  defp next(%{state: :init} = engine) do
    transition(engine, :resolve_fields)
  end

  defp next(%{state: :resolve_fields} = engine) do
    resolve_fields(engine)
  end

  defp next(%{state: :strict_check} = engine) do
    strict_check(engine)
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
    case choose_request_to_resolve(engine) do
      {:ok, request, other_requests} ->
        {engine, request} = resolve_data(engine, request)
        transition(engine, :strict_check, %{requests: [request | other_requests]})

      :error ->
        transition(engine, :complete, %{message: "No requests to resolve"})
    end
  end

  defp next(%{state: :resolve_complete} = engine) do
    {engine, requests} =
      Enum.reduce(engine.requests, {engine, []}, fn request, {engine, requests} ->
        if request.resolve_when_fetch_only? ||
             Enum.any?(engine.requests, fn other_request ->
               other_request.resolve_when_fetch_only? &&
                 Request.depends_on?(other_request, request)
             end) do
          {engine, request} = resolve_data(engine, request)

          {engine, [request | requests]}
        else
          {engine, [request | requests]}
        end
      end)

    transition(engine, :complete, %{requests: Enum.reverse(requests)})
  end

  defp choose_request_to_resolve(engine) do
    {can_resolve_data, cant_resolve_data} =
      Enum.split_with(engine.requests, fn request ->
        not request.strict_access? && match?(%Request.UnresolvedField{}, request.data) &&
          Request.all_dependencies_met?(request, engine.data)
      end)

    case can_resolve_data do
      [request | rest] -> {:ok, request, rest ++ cant_resolve_data}
      [] -> :error
    end
  end

  defp resolve_data(engine, request) do
    case Request.resolve_data(engine.data, request) do
      {:ok, resolved} ->
        {%{engine | data: put_nested_path(engine.data, request.path, resolved.data)}, resolved}

      {:error, error} ->
        {%{engine | errors: put_nested_path(engine.errors, request.path, error)}, request}
    end
  end

  defp resolve_fields(engine) do
    {errors, new_requests} =
      engine.requests
      |> Enum.map(&Request.resolve_fields(&1, engine.data))
      |> find_errors()

    cond do
      new_requests == engine.requests ->
        transition(engine, :strict_check, %{
          message: "Resolving resulted in no changes",
          errors: errors
        })

      true ->
        remain(engine, %{
          errors: errors,
          requests: new_requests,
          message: "Resolved fields. Triggering another pass."
        })
    end
  end

  defp find_errors(requests) do
    {errors, good_requests} =
      Enum.reduce(requests, {[], []}, fn request, {errors, good_requests} ->
        case Request.errors(request) do
          request_errors when request_errors == %{} ->
            {errors, [request | good_requests]}

          request_errors ->
            new_request_errors =
              Enum.reduce(request_errors, errors, fn {key, error}, request_error_acc ->
                [{[request.name, key], error} | request_error_acc]
              end)

            {new_request_errors, good_requests}
        end
      end)

    {errors, Enum.reverse(good_requests)}
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

    case SatSolver.solve2(rules_with_data, engine.facts) do
      {:ok, scenarios} ->
        transition(engine, :reality_check, %{scenarios: scenarios})

      {:error, :unsatisfiable} ->
        # TODO: Errors
        transition(engine, :complete, %{errors: {:__engine__, "Unauthorized"}})
    end
  end

  defp get_pkeys(%resource{} = item, api) do
    pkey_filter =
      item
      |> Map.take(Ash.primary_key(resource))
      |> Map.to_list()

    Ash.Filter.parse(resource, pkey_filter, api)
  end

  defp strict_check(engine) do
    {requests, facts} =
      Enum.reduce(engine.requests, {[], engine.facts}, fn request, {requests, facts} ->
        {new_request, new_facts} =
          Ash.Authorization.Checker.strict_check2(
            engine.user,
            request,
            facts
          )

        {[new_request | requests], new_facts}
      end)

    transition(engine, :generate_scenarios, %{requests: Enum.reverse(requests), facts: facts})
  end

  defp new(requests, api, opts) do
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
        "Initializing engine with requests: #{Enum.map_join(requests, ", ", & &1.name)}"
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

  defp handle_errors(engine, %{errors: error}) when not is_list(error) do
    handle_errors(engine, %{errors: List.wrap(error)})
  end

  defp handle_errors(engine, %{errors: errors}) when errors != [] do
    Enum.reduce(errors, engine, fn {path, error}, engine ->
      %{engine | errors: put_nested_path(engine.errors, List.wrap(path), error)}
    end)
  end

  defp handle_errors(engine, _), do: engine

  defp put_nested_path(errors, path, error) when is_list(errors) do
    Enum.map(errors, &put_nested_path(&1, path, error))
  end

  defp put_nested_path(errors, [key], error) do
    case errors do
      %{^key => value} when is_list(value) -> Map.put(errors, key, [error | value])
      value when is_map(value) -> Map.put(value, key, error)
    end
  end

  defp put_nested_path(errors, [key | rest], error) do
    Map.put(errors, key, put_nested_path(Map.get(errors, key, %{}), rest, error))
  end
end
