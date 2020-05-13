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
    :verbose?,
    :failure_mode,
    :pubsub_adapter,
    errors: [],
    completed_preparations: %{},
    data: %{},
    state: :init,
    authorized?: false,
    facts: %{
      true: true,
      false: false
    },
    no_resolvable_requests?: false,
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
      %{state: :complete, authorized?: false} = new_engine ->
        error =
          Ash.Error.Forbidden.exception(
            requests: engine.requests,
            facts: engine.facts,
            state: engine.data,
            api: engine.api
          )

        if engine.verbose? do
          report = Ash.Authorization.Report.report_from_engine(new_engine)
          Logger.info(report)
        end

        add_error(new_engine, :__engine__, error)

      %{state: :complete} = new_engine ->
        if engine.verbose? do
          report = Ash.Authorization.Report.report_from_engine(new_engine)
          Logger.info(report)
        end

        new_engine

      new_engine when new_engine == engine ->
        transition(new_engine, :complete, %{
          errors: {:__engine__, "State machine stuck in infinite loop"}
        })

      new_engine ->
        loop_until_complete(new_engine)
    end
  end

  defp next(%{failure_mode: :complete, errors: errors} = engine) when errors != [] do
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

  defp next(%{state: :check, authorized?: true} = engine) do
    transition(engine, :resolve_complete, %{message: "Request is already authorized"})
  end

  defp next(%{state: :check} = engine) do
    engine
    |> strict_check()
    |> check()
    |> transition(:generate_scenarios)
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
        # case Enum.find(engine.requests, &)
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
      Request.all_dependencies_met?(request, engine.data, true) &&
        (request.resolve_when_fetch_only? || is_hard_depended_on?(request, engine.requests))
    end
  end

  defp is_hard_depended_on?(request, requests) do
    requests
    |> Enum.reject(& &1.error?)
    |> Enum.reject(&Request.data_resolved?/1)
    |> Enum.filter(&Request.depends_on?(&1, request))
    |> Enum.any?(fn other_request ->
      other_request.resolve_when_fetch_only?
    end)
  end

  defp prepare(_, %{prepared?: true} = request), do: {:ok, request}

  defp prepare(engine, request) do
    # Right now the only preparation is a side_load
    empty_query = Ash.Query.new(engine.api, request.resource)

    side_load_query =
      Enum.reduce(request.rules, empty_query, fn {_, clause}, query ->
        clause.check_opts
        |> clause.check_module.prepare()
        |> Enum.reduce(query, fn {:side_load, path}, query ->
          Ash.Query.side_load(query, path)
        end)
      end)

    case Ash.Actions.SideLoad.side_load(
           request.data,
           side_load_query
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
    # TODO: Sort these by whether or not their optional deps have been met
    # perhaps by the count of unmet optional deps?
    # Also, sort them by whether or not they *should* be fetched
    engine.requests
    |> Enum.reject(& &1.error?)
    |> Enum.filter(&Request.all_dependencies_met?(&1, engine.data, true))
    |> Enum.filter(&allowed_access?(engine, &1))
  end

  defp allowed_access?(engine, request) do
    if request.strict_access? do
      false
    else
      could_pass_strict_check_in_isolation?(request, engine)
    end
  end

  defp could_pass_strict_check_in_isolation?(request, engine) do
    requests_with_data_filter =
      Enum.flat_map([request], fn request ->
        if Request.data_resolved?(request) && request.data not in [nil, []] do
          request.data
          |> List.wrap()
          |> Enum.map(fn item ->
            %{request | query: get_pkeys(request, engine.api, item)}
          end)
        else
          [request]
        end
      end)

    case SatSolver.solve(requests_with_data_filter, engine.facts) do
      {:ok, _scenarios} ->
        true

      {:error, :unsatisfiable} ->
        false
    end
  end

  defp resolve_data(engine, request) do
    if engine.verbose? do
      Logger.debug("Resolving data #{request.name}")
    end

    with {:ok, engine, request} <- resolve_dependencies(request, engine, true),
         engine <- replace_request(engine, request),
         {:ok, resolved_request} <- Request.resolve_data(engine.data, request),
         engine <- replace_request(engine, resolved_request),
         {:ok, prepared_request} <- prepare(engine, resolved_request) do
      query =
        if prepared_request.action_type == :create do
          get_pkeys(prepared_request, engine.api, prepared_request.data)
        else
          prepared_request.query
        end

      replace_request(engine, %{prepared_request | query: query})
    else
      {:error, %__MODULE__{} = engine} ->
        request = Enum.find(engine.requests, &(&1.id == request.id))
        replace_request(engine, %{request | error?: true})

      {:error, error} ->
        request = Enum.find(engine.requests, &(&1.id == request.id))
        new_request = %{request | error?: true}

        engine
        |> replace_request(new_request)
        |> add_error(request.path ++ [:data], error)
    end
  end

  defp reality_check(%{authorized?: true} = engine) do
    transition(engine, :resolve_complete)
  end

  defp reality_check(engine) do
    case find_real_scenario(engine.scenarios, engine.facts) do
      nil ->
        transition(engine, :resolve_some, %{message: "No scenario was reality"})

      scenario ->
        scenario = Map.drop(scenario, [true, false])

        engine
        |> Map.put(:authorized?, true)
        |> transition(:resolve_complete, %{
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
    requests_with_data_filter =
      Enum.flat_map(engine.requests, fn request ->
        if Request.data_resolved?(request) && request.data not in [nil, []] do
          request.data
          |> List.wrap()
          |> Enum.map(fn item ->
            %{request | query: get_pkeys(request, engine.api, item)}
          end)
        else
          [request]
        end
      end)

    case SatSolver.solve(requests_with_data_filter, engine.facts) do
      {:ok, scenarios} ->
        transition(engine, :reality_check, %{scenarios: scenarios})

      {:error, :unsatisfiable} ->
        engine
        |> Map.put(:authorized?, false)
        |> transition(:complete)
    end
  end

  def get_pkeys(%{query: nil, resource: resource}, api, %_{} = item) do
    pkey_filter =
      item
      |> Map.take(Ash.primary_key(resource))
      |> Map.to_list()

    api
    |> Ash.Query.new(resource)
    |> Ash.Query.filter(pkey_filter)
  end

  def get_pkeys(%{query: query}, _, %resource{} = item) do
    pkey_filter =
      item
      |> Map.take(Ash.primary_key(resource))
      |> Map.to_list()

    Ash.Query.filter(query, pkey_filter)
  end

  defp check(engine) do
    case do_check(engine) do
      {:cont, engine} -> check(engine)
      {:halt, engine} -> engine
    end
  end

  defp do_check(engine) do
    case checkable_requests(engine) do
      [] ->
        {:halt, engine}

      requests ->
        {new_requests, new_engine} =
          Enum.reduce(requests, {[], engine}, fn request, {new_requests, engine} ->
            case resolve_dependencies(request, engine, true) do
              {:ok, new_engine, new_request} ->
                {[new_request | new_requests], new_engine}

              {:error, new_engine} ->
                {new_requests, new_engine}
            end
          end)

        case new_requests do
          [new_request | _others] ->
            if engine.verbose? do
              Logger.debug("Checking #{new_request.name}")
            end

            {:cont, run_checks(new_engine, new_request)}

          [] ->
            {:halt, new_engine}
        end
    end
  end

  defp run_checks(engine, request) do
    request.rules
    |> Enum.filter(fn {_kind, clause} ->
      case Clause.find(engine.facts, clause) do
        {:ok, :unknown} -> true
        {:ok, :unknowable} -> false
        {:ok, _} -> false
        :error -> true
      end
    end)
    |> Enum.reduce_while({:ok, engine}, fn {_kind, clause}, {:ok, engine} ->
      # TODO: remove the third argument below/just pass in the request
      case Checker.run_checks(engine, request, clause) do
        {:error, error} ->
          engine = add_error(engine, request.path, error)

          {:halt, {:error, engine}}

        {:ok, new_engine} ->
          {:cont, {:ok, new_engine}}
      end
    end)
    |> case do
      {:error, engine} ->
        replace_request(engine, %{request | error?: true, check_complete?: true})

      {:ok, new_engine} ->
        replace_request(new_engine, %{request | check_complete?: true})
    end
  end

  defp checkable_requests(engine) do
    engine.requests
    |> Enum.filter(& &1.strict_check_complete?)
    |> Enum.filter(&Request.data_resolved?/1)
    |> Enum.reject(& &1.check_complete?)
    |> Enum.reject(& &1.error?)
  end

  defp strict_check(engine) do
    case do_strict_check(engine) do
      {:cont, engine} -> strict_check(engine)
      {:halt, engine} -> engine
    end
  end

  defp do_strict_check(engine) do
    engine.requests
    |> Enum.filter(&Request.can_strict_check(&1, engine.data))
    |> case do
      [] ->
        {:halt, engine}

      requests ->
        {new_requests, new_engine} =
          Enum.reduce(requests, {[], engine}, fn request, {requests, engine} ->
            case resolve_dependencies(request, engine, false) do
              {:ok, new_engine, new_request} ->
                {[new_request | requests], new_engine}

              {:error, engine} ->
                {requests,
                 replace_request(engine, %{
                   request
                   | strict_check_complete?: true,
                     error?: true
                 })}
            end
          end)

        Enum.reduce(new_requests, {:cont, new_engine}, fn request, {:cont, engine} ->
          if engine.verbose? do
            Logger.debug("Strict checking #{request.name}")
          end

          {new_request, new_facts} = Checker.strict_check(engine.user, request, engine.facts)

          new_engine =
            new_engine
            |> replace_request(new_request)
            |> Map.put(:facts, new_facts)

          {:cont, new_engine}
        end)
    end
  end

  defp resolve_dependencies(request, engine, data?) do
    case do_resolve_dependencies(request, engine, data?) do
      :done ->
        {:ok, engine, request}

      {:ok, engine, request} ->
        resolve_dependencies(request, engine, data?)

      {:error, engine, dep, error} ->
        {:error, add_error(engine, dep, error)}
    end
  end

  defp do_resolve_dependencies(request, engine, data?) do
    request
    |> Map.from_struct()
    |> Enum.filter(&match?({_, %Request.UnresolvedField{}}, &1))
    |> Enum.find(fn {_, unresolved} ->
      if unresolved.data? and not data? and
           Request.all_dependencies_met?(request, engine.data, false) do
        false
      else
        true
      end
    end)
    |> case do
      nil ->
        :done

      {key, unresolved} ->
        case resolve_field(engine, unresolved, data?) do
          {:ok, new_engine, resolved} ->
            new_request = Map.put(request, key, resolved)
            new_engine = replace_request(new_engine, new_request)
            {:ok, new_engine, new_request}

          {:error, new_engine, dep, error} ->
            new_engine = replace_request(new_engine, %{request | error?: true})
            {:error, new_engine, dep || request.path ++ [key], error}
        end
    end
  end

  defp resolve_field(engine, %{deps: deps} = unresolved, data?, dep \\ nil) do
    result =
      Enum.reduce_while(deps, {:ok, engine}, fn dep, {:ok, engine} ->
        # TODO: this is inneficient
        path = :lists.droplast(dep)
        key = List.last(dep)

        other_request = Enum.find(engine.requests, &(&1.path == path))

        case Map.get(other_request, key) do
          %Request.UnresolvedField{data?: field_is_data?} = other_value ->
            if field_is_data? and not data? do
              {:cont, {:ok, engine}}
            else
              case resolve_field(engine, other_value, data?, dep) do
                {:ok, new_engine, resolved} ->
                  new_request = Map.put(other_request, key, resolved)
                  {:cont, {:ok, replace_request(new_engine, new_request)}}

                {:error, engine, dep, error} ->
                  {:halt, {:error, engine, dep, error}}
              end
            end

          _ ->
            {:cont, {:ok, engine}}
        end
      end)

    case result do
      {:ok, new_engine} ->
        if unresolved.data? and not data? do
          {:ok, new_engine, unresolved}
        else
          case Request.resolve_field(new_engine.data, unresolved) do
            {:ok, value} ->
              {:ok, new_engine, value}

            {:error, error} ->
              {:error, engine, dep, error}
          end
        end

      {:error, engine, dep, error} ->
        {:error, engine, dep, error}
    end
  end

  defp new(request, api, opts) when not is_list(request), do: new([request], api, opts)

  defp new(requests, api, opts) do
    requests
    |> new_engine(api, opts)
    |> add_default_queries()
    |> validate_unique_paths()
    |> bypass_strict_access(opts)
    |> validate_dependencies()
    |> validate_has_rules()
    |> log_init()
  end

  defp add_default_queries(engine) do
    %{
      engine
      | requests:
          Enum.map(engine.requests, fn request ->
            %{request | query: request.query || Ash.Query.new(engine.api, request.resource)}
          end)
    }
  end

  defp validate_dependencies(engine) do
    case Request.build_dependencies(engine.requests) do
      :impossible ->
        add_error(engine, [:__engine__], "Request dependencies are not possible")

      {:ok, _requests} ->
        # TODO: no need to aggregate the full dependencies of
        engine
    end
  end

  defp validate_has_rules(%{authorized?: true} = engine), do: engine

  defp validate_has_rules(engine) do
    case Enum.find(engine.requests, &Enum.empty?(&1.rules)) do
      nil ->
        engine

      request ->
        add_error(engine, request.path, "No authorization steps configured")
    end
  end

  defp validate_unique_paths(engine) do
    case Request.validate_unique_paths(engine.requests) do
      :ok ->
        engine

      {:error, paths} ->
        Enum.reduce(paths, engine, &add_error(&2, &1, "Duplicate requests at path"))
    end
  end

  defp new_engine(requests, api, opts) do
    %__MODULE__{
      requests: requests,
      user: opts[:user],
      api: api,
      authorized?: !!opts[:fetch_only?],
      failure_mode: opts[:failure_mode] || :complete,
      verbose?: Keyword.get(opts, :verbose?, false)
    }
  end

  defp log_init(engine) do
    if engine.verbose? do
      Logger.debug(
        "Initializing engine with requests: #{
          Enum.map_join(engine.requests, ", ", &(to_string(&1.resource) <> ": " <> &1.name))
        }"
      )
    end

    engine
  end

  defp bypass_strict_access(engine, opts) do
    if opts[:bypass_strict_access?] do
      %{engine | requests: Enum.map(engine.requests, &Map.put(&1, :strict_access?, false))}
    else
      engine
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
    if engine.verbose? do
      Logger.debug("Remaining in #{engine.state}#{format_args(args)}")
    end

    engine
    |> handle_args(args)
  end

  defp transition(engine, state, args \\ %{}) do
    if engine.verbose? do
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

  defp add_error(engine, path, error) do
    path = List.wrap(path)
    error = to_ash_error(error)

    %{engine | errors: [Map.put(error, :path, path) | engine.errors]}
  end

  def to_ash_error(error) do
    if Ash.ash_error?(error) do
      error
    else
      Ash.Error.Unknown.exception(error: error)
    end
  end
end
