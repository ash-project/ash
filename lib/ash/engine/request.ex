defmodule Ash.Engine.Request do
  @moduledoc false

  alias Ash.Error.Forbidden.MustPassStrictCheck
  alias Ash.Error.Framework.AssumptionFailed
  alias Ash.Error.Invalid.{DuplicatedPath, ImpossiblePath}

  defmodule UnresolvedField do
    @moduledoc false
    defstruct [:resolver, deps: [], data?: false]

    def new(dependencies, func) do
      %__MODULE__{
        resolver: func,
        deps: deps(dependencies)
      }
    end

    defp deps(deps) do
      deps
      |> List.wrap()
      |> Enum.map(fn dep -> List.wrap(dep) end)
    end
  end

  defimpl Inspect, for: UnresolvedField do
    import Inspect.Algebra

    def inspect(field, opts) do
      concat([
        "#UnresolvedField<",
        to_doc(field.deps, opts),
        ">"
      ])
    end
  end

  defstruct [
    :id,
    :async?,
    :error?,
    :resource,
    :changeset,
    :path,
    :action_type,
    :action,
    :data,
    :name,
    :api,
    :query,
    :authorization_filter,
    :write_to_data?,
    :strict_check_only?,
    :verbose?,
    :state,
    :actor,
    :authorize?,
    :engine_pid,
    authorized?: false,
    authorizer_state: %{},
    dependencies_to_send: %{},
    dependency_data: %{},
    dependencies_requested: []
  ]

  @type t :: %__MODULE__{}

  require Logger

  alias Ash.Authorizer

  def resolve(dependencies \\ [], func) do
    UnresolvedField.new(dependencies, func)
  end

  def new(opts) do
    query =
      case opts[:query] do
        %UnresolvedField{} = query ->
          query

        %Ash.Query{} = query ->
          query

        nil ->
          nil

        other ->
          raise "Got a weird thing #{inspect(other)}"
      end

    id = Ecto.UUID.generate()

    data =
      case opts[:data] do
        %UnresolvedField{} = unresolved ->
          %{unresolved | data?: true}

        other ->
          other
      end

    %__MODULE__{
      id: id,
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: List.wrap(opts[:path]),
      action_type: opts[:action_type],
      action: opts[:action],
      async?: Keyword.get(opts, :async?, true),
      data: data,
      query: query,
      api: opts[:api],
      name: opts[:name],
      strict_check_only?: opts[:strict_check_only?],
      state: :strict_check,
      actor: opts[:actor],
      authorized?: opts[:authorize?] == false,
      verbose?: opts[:verbose?] || false,
      authorize?: opts[:authorize?] || true,
      write_to_data?: Keyword.get(opts, :write_to_data?, true)
    }
  end

  def next(request) do
    case do_next(request) do
      {:complete, new_request, notifications, dependencies} ->
        if request.state != :complete do
          {:complete, new_request, notifications, dependencies}
        else
          {:already_complete, new_request, notifications, dependencies}
        end

      {:waiting, new_request, notifications, dependencies} ->
        {:wait, new_request, notifications, dependencies}

      {:continue, new_request, notifications} ->
        {:continue, new_request, notifications}

      {:error, error, request} ->
        {:error, error, request}
    end
  end

  def do_next(%{state: :strict_check, authorize?: false} = request) do
    log(request, "Skipping strict check due to authorize?: false")
    {:continue, %{request | state: :fetch_data}, []}
  end

  def do_next(%{state: :strict_check} = request) do
    case Ash.Resource.authorizers(request.resource) do
      [] ->
        log(request, "No authorizers found, skipping strict check")
        {:continue, %{request | state: :fetch_data}, []}

      authorizers ->
        case strict_check(authorizers, request) do
          {:ok, new_request, notifications, []} ->
            new_request = set_authorized(new_request)
            log(new_request, "Strict check complete")
            {:continue, %{new_request | state: :fetch_data}, notifications}

          {:ok, new_request, notifications, dependencies} ->
            log(new_request, "Strict check incomplete, waiting on dependencies")
            {:waiting, new_request, notifications, dependencies}

          {:error, error} ->
            log(request, "Strict checking failed")
            {:error, error, request}
        end
    end
  end

  def do_next(%{state: :fetch_data} = request) do
    case try_resolve_local(request, :data, true) do
      {:skipped, _, _, _} ->
        {:error, AssumptionFailed.exception(message: "Skipped fetching data"), request}

      {:ok, request, notifications, []} ->
        log(request, "data fetched: #{inspect(notifications)}")
        {:continue, %{request | state: :check}, notifications}

      {:ok, new_request, notifications, waiting_for} ->
        log(request, "data waiting on dependencies: #{inspect(waiting_for)}")
        {:waiting, new_request, notifications, waiting_for}

      {:error, error} ->
        log(request, "error fetching data: #{inspect(error)}")
        {:error, error, request}
    end
  end

  def do_next(%{state: :check, authorize?: false} = request) do
    log(request, "Skipping check due to `authorize?: false`")
    {:complete, %{request | state: :complete}, [], []}
  end

  def do_next(%{state: :check} = request) do
    case Ash.Resource.authorizers(request.resource) do
      [] ->
        log(request, "No authorizers found, skipping check")
        {:complete, %{request | state: :complete}, [], []}

      authorizers ->
        case check(authorizers, request) do
          {:ok, new_request, notifications, []} ->
            log(new_request, "Check complete")
            new_request = set_authorized(new_request)

            {:complete, %{new_request | state: :complete}, notifications, []}

          {:ok, new_request, notifications, waiting} ->
            log(request, "Check incomplete, waiting on dependencies")
            {:waiting, new_request, notifications, waiting}

          {:error, error} ->
            log(request, "Check failed")
            {:error, error, request}
        end
    end
  end

  def do_next(%{state: :complete} = request) do
    if request.dependencies_to_send == %{} do
      {:complete, request, [], []}
    else
      Enum.reduce_while(request.dependencies_to_send, {:complete, request, [], []}, fn
        {field, _paths}, {:complete, request, notifications, deps} ->
          case try_resolve_local(request, field, false) do
            {:skipped, new_request, new_notifications, other_deps} ->
              {:cont,
               {:complete, new_request, new_notifications ++ notifications, other_deps ++ deps}}

            {:ok, new_request, new_notifications, other_deps} ->
              {:cont,
               {:complete, new_request, new_notifications ++ notifications, other_deps ++ deps}}

            {:error, error} ->
              {:halt, {:error, error, request}}
          end
      end)
    end
  end

  def wont_receive(request, path, field) do
    log(request, "Request failed due to failed dependency #{inspect(path ++ [field])}")

    {:stop, :dependency_failed, request}
  end

  def send_field(request, receiver_path, field) do
    log(request, "Attempting to provide #{inspect(field)} for #{inspect(receiver_path)}")

    case store_dependency(request, receiver_path, field) do
      {:value, value, new_request} ->
        {:ok, new_request, [{receiver_path, request.path, field, value}]}

      {:ok, new_request, notifications} ->
        {:ok, new_request, notifications}

      {:waiting, new_request, notifications, []} ->
        {:ok, new_request, notifications}

      {:waiting, new_request, notifications, waiting_for} ->
        {:waiting, new_request, notifications, waiting_for}

      {:error, error, new_request} ->
        log(request, "Error resolving #{field}: #{inspect(error)}")

        {:error, error, new_request}
    end
  end

  def receive_field(request, path, field, value) do
    log(request, "Receiving field #{field} from #{inspect(path)}")

    new_request = put_dependency_data(request, path ++ [field], value)

    {:continue, new_request}
  end

  defp set_authorized(%{authorized?: false, resource: resource} = request) do
    authorized? =
      resource
      |> Ash.Resource.authorizers()
      |> Enum.all?(fn authorizer ->
        authorizer_state(request, authorizer) == :authorizer
      end)

    %{request | authorized?: authorized?}
  end

  defp set_authorized(request), do: request

  def put_dependency_data(request, dep, value) do
    %{request | dependency_data: Map.put(request.dependency_data, dep, value)}
  end

  def store_dependency(request, receiver_path, field, internal? \\ false) do
    request = do_store_dependency(request, field, receiver_path)

    case try_resolve_local(request, field, internal?) do
      {:skipped, new_request, notifications, []} ->
        log(request, "Field #{field} was skipped, no additional dependencies")
        {:ok, new_request, notifications}

      {:skipped, new_request, notifications, waiting} ->
        log(request, "Field #{field} was skipped, registering dependencies: #{inspect(waiting)}")

        {:waiting, new_request, notifications, waiting}

      {:ok, new_request, _, _} ->
        case Map.get(new_request, field) do
          %UnresolvedField{} ->
            log(request, "Field could not be resolved #{field}, registering dependency")
            {:ok, new_request, []}

          value ->
            log(request, "Field #{field}, was resolved and provided")
            {:value, value, new_request}
        end

      {:error, error} ->
        {:error, error, request}
    end
  end

  defp do_store_dependency(request, field, receiver_path) do
    log(request, "storing dependency on #{field} from #{inspect(receiver_path)}")

    new_deps_to_send =
      Map.update(request.dependencies_to_send, field, [receiver_path], fn paths ->
        paths = Enum.reject(paths, &Kernel.==(&1, receiver_path))
        [receiver_path | paths]
      end)

    %{request | dependencies_to_send: new_deps_to_send}
  end

  defp strict_check(authorizers, request) do
    authorizers
    |> Enum.reject(&(authorizer_state(request, &1) == :authorized))
    |> Enum.reduce_while({:ok, request, [], []}, fn authorizer,
                                                    {:ok, request, notifications, waiting_for} ->
      log(request, "strict checking")

      case do_strict_check(authorizer, request) do
        {:ok, new_request} ->
          log(new_request, "strict check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_request, notifications, waiting_for}}

        {:ok, new_request, new_notifications, new_deps} ->
          log(new_request, "strict check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_request, new_notifications ++ notifications, waiting_for ++ new_deps}}

        {:waiting, new_request, new_notifications, new_deps} ->
          log(
            new_request,
            "waiting on dependencies: #{inspect(new_deps)} for #{inspect(authorizer)}"
          )

          {:cont, {:ok, new_request, notifications ++ new_notifications, new_deps ++ waiting_for}}

        {:error, error} ->
          log(request, "strict check failed for #{inspect(authorizer)}: #{inspect(error)}")

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_strict_check(authorizer, request, notifications \\ []) do
    strict_check_only? = request.strict_check_only?

    case missing_strict_check_dependencies?(authorizer, request) do
      [] ->
        case strict_check_authorizer(authorizer, request) do
          :authorized ->
            {:ok, set_authorizer_state(request, authorizer, :authorized)}

          {:filter, filter} ->
            request
            |> Map.update!(:query, &Ash.Query.filter(&1, filter))
            |> Map.update(
              :authorization_filter,
              filter,
              &add_to_or_parse(&1, filter, request.resource)
            )
            |> set_authorizer_state(authorizer, :authorized)
            |> try_resolve([request.path ++ [:query]], false)

          {:filter_and_continue, _, _} when strict_check_only? ->
            {:error, MustPassStrictCheck.exception(resource: request.resource)}

          {:filter_and_continue, filter, new_authorizer_state} ->
            new_request =
              request
              |> Map.update!(:query, &Ash.Query.filter(&1, filter))
              |> Map.update(:authorization_filter, filter, &Ash.Filter.add_to_filter(&1, filter))
              |> set_authorizer_state(authorizer, new_authorizer_state)

            {:ok, new_request}

          {:continue, _} when strict_check_only? ->
            {:error, MustPassStrictCheck.exception(resource: request.resource)}

          {:continue, authorizer_state} ->
            {:ok, set_authorizer_state(request, authorizer, authorizer_state)}

          {:error, error} ->
            {:error, error}
        end

      deps ->
        deps =
          Enum.map(deps, fn dep ->
            request.path ++ [dep]
          end)

        case try_resolve(request, deps, true) do
          {:ok, new_request, new_notifications, []} ->
            do_strict_check(authorizer, new_request, notifications ++ new_notifications)

          {:ok, new_request, new_notifications, waiting_for} ->
            {:waiting, new_request, notifications ++ new_notifications, waiting_for}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp add_to_or_parse(existing_authorization_filter, filter, resource) do
    if existing_authorization_filter do
      Ash.Filter.add_to_filter(existing_authorization_filter, filter)
    else
      Ash.Filter.parse!(resource, filter)
    end
  end

  defp check(authorizers, request) do
    authorizers
    |> Enum.reject(&(authorizer_state(request, &1) == :authorized))
    |> Enum.reduce_while({:ok, request, [], []}, fn authorizer,
                                                    {:ok, request, notifications, waiting_for} ->
      case do_check(authorizer, request) do
        {:ok, new_request} ->
          log(request, "check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_request, notifications, waiting_for}}

        {:ok, new_request, new_notifications, new_deps} ->
          log(request, "check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_request, new_notifications ++ notifications, new_deps ++ waiting_for}}

        {:waiting, new_request, new_notifications, new_deps} ->
          log(
            request,
            "waiting on dependencies: #{inspect(new_deps)} for #{inspect(authorizer)}"
          )

          {:cont, {:ok, new_request, new_notifications ++ notifications, new_deps ++ waiting_for}}

        {:error, error} ->
          log(request, "check failed for #{inspect(authorizer)}: #{inspect(error)}")

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_check(authorizer, request, notifications \\ []) do
    case missing_check_dependencies(authorizer, request) do
      [] ->
        case check_authorizer(authorizer, request) do
          :authorized ->
            {:ok, set_authorizer_state(request, authorizer, :authorized)}

          {:filter, filter} ->
            request
            |> set_authorizer_state(authorizer, :authorized)
            |> Map.update(
              :authorization_filter,
              filter,
              &Ash.Filter.add_to_filter(&1, filter)
            )
            |> runtime_filter(authorizer, filter)

          {:error, error} ->
            {:error, error}
        end

      deps ->
        deps =
          Enum.map(deps, fn dep ->
            request.path ++ [dep]
          end)

        case try_resolve(request, deps, true) do
          {:ok, new_request, new_notifications, []} ->
            do_check(authorizer, new_request, notifications ++ new_notifications)

          {:ok, new_request, new_notifications, waiting_for} ->
            {:waiting, new_request, new_notifications ++ notifications, waiting_for}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp runtime_filter(request, authorizer, filter) do
    case do_runtime_filter(request, filter) do
      {:ok, request} ->
        request
        |> set_authorizer_state(authorizer, :authorized)
        |> try_resolve([request.path ++ [:data], request.path ++ [:query]], false)

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_runtime_filter(%{data: empty} = request, _filter) when empty in [nil, []],
    do: {:ok, request}

  defp do_runtime_filter(request, filter) do
    pkey = Ash.Resource.primary_key(request.resource)

    pkeys =
      request.data
      |> List.wrap()
      |> Enum.map(fn record ->
        record |> Map.take(pkey) |> Map.to_list()
      end)

    primary_key_filter =
      case pkeys do
        [pkey] -> [pkey]
        pkeys -> [or: pkeys]
      end

    new_query =
      request.query
      |> Ash.Query.filter(primary_key_filter)
      |> Ash.Query.filter(filter)

    request.api.read(new_query)
    |> case do
      {:ok, results} ->
        pkey = Ash.Resource.primary_key(request.resource)
        pkeys = Enum.map(results, &Map.take(&1, pkey))

        new_data = Enum.filter(request.data, &(Map.take(&1, pkey) in pkeys))

        {:ok, %{request | data: new_data, query: new_query}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp try_resolve(request, deps, internal?) do
    Enum.reduce_while(deps, {:ok, request, [], []}, fn dep,
                                                       {:ok, request, notifications, skipped} ->
      case get_dependency_data(request, dep) do
        {:ok, _value} ->
          {:cont, {:ok, request, notifications, skipped}}

        :error ->
          do_try_resolve(request, notifications, skipped, dep, internal?)
      end
    end)
  end

  defp do_try_resolve(request, notifications, skipped, dep, internal?) do
    if local_dep?(request, dep) do
      case try_resolve_local(request, List.last(dep), internal?) do
        {:skipped, request, new_notifications, other_deps} ->
          {:cont, {:ok, request, new_notifications ++ notifications, skipped ++ other_deps}}

        {:ok, request, new_notifications, other_deps} ->
          {:cont, {:ok, request, new_notifications ++ notifications, skipped ++ other_deps}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    else
      {:cont, {:ok, request, notifications, [dep | skipped]}}
    end
  end

  defp try_resolve_local(request, field, internal?) do
    authorized? = Enum.all?(Map.values(request.authorizer_state), &(&1 == :authorized))

    # Don't fetch honor requests for dat until the request is authorized
    if field in [:data, :query] and not authorized? and not internal? do
      try_resolve_dependencies_of(request, field, internal?)
    else
      case Map.get(request, field) do
        %UnresolvedField{} = unresolved ->
          do_try_resolve_local(request, field, unresolved, internal?)

        value ->
          notify_existing_value(request, field, value, internal?)
      end
    end
  end

  defp try_resolve_dependencies_of(request, field, internal?) do
    case Map.get(request, field) do
      %UnresolvedField{deps: deps} ->
        case try_resolve(request, deps, internal?) do
          {:ok, new_request, notifications, remaining_deps} ->
            {:skipped, new_request, notifications, remaining_deps}

          error ->
            error
        end

      _ ->
        {:skipped, request, [], []}
    end
  end

  defp notify_existing_value(request, field, value, internal?) do
    if internal? do
      {new_request, notifications} = notifications(request, field, value)

      {:ok, new_request, notifications, []}
    else
      {:ok, request, [], []}
    end
  end

  defp do_try_resolve_local(request, field, unresolved, internal?) do
    %{deps: deps, resolver: resolver} = unresolved

    with {:ok, new_request, notifications, []} <-
           try_resolve(request, deps, internal?) do
      resolver_context = resolver_context(new_request, deps)

      log(request, "resolving #{field}")

      case resolver.(resolver_context) do
        {:ok, value} ->
          {new_request, notifications} =
            if internal? do
              {new_request, new_notifications} = notifications(new_request, field, value)

              notifications =
                Enum.concat([
                  notifications,
                  new_notifications
                ])

              {new_request, notifications}
            else
              {request, []}
            end

          new_request = Map.put(new_request, field, value)
          {:ok, new_request, notifications, []}

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp get_dependency_data(request, dep) do
    if local_dep?(request, dep) do
      case Map.fetch(request, List.last(dep)) do
        {:ok, %UnresolvedField{}} -> :error
        {:ok, value} -> {:ok, value}
        :error -> :error
      end
    else
      Map.fetch(request.dependency_data, dep)
    end
  end

  defp notifications(request, field, value) do
    case Map.fetch(request.dependencies_to_send, field) do
      {:ok, paths} ->
        new_request = %{
          request
          | dependencies_to_send: Map.delete(request.dependencies_to_send, field)
        }

        notifications =
          Enum.map(paths, fn path ->
            {path, request.path, field, value}
          end)

        {new_request, notifications}

      :error ->
        {request, []}
    end
  end

  defp resolver_context(request, deps) do
    Enum.reduce(deps, %{}, fn dep, resolver_context ->
      case get_dependency_data(request, dep) do
        {:ok, value} ->
          Ash.Engine.put_nested_key(resolver_context, dep, value)

        :error ->
          resolver_context
      end
    end)
  end

  defp local_dep?(request, dep) do
    :lists.droplast(dep) == request.path
  end

  def add_initial_authorizer_state(request) do
    request.resource
    |> Ash.Resource.authorizers()
    |> Enum.reduce(request, fn authorizer, request ->
      if request.authorize? do
        initial_state =
          Authorizer.initial_state(
            authorizer,
            request.actor,
            request.resource,
            request.action,
            request.verbose?
          )

        set_authorizer_state(request, authorizer, initial_state)
      else
        set_authorizer_state(request, authorizer, :authorized)
      end
    end)
  end

  defp missing_strict_check_dependencies?(authorizer, request) do
    authorizer
    |> Authorizer.strict_check_context(authorizer_state(request, authorizer))
    |> Enum.filter(fn dependency ->
      match?(%UnresolvedField{}, Map.get(request, dependency))
    end)
  end

  defp missing_check_dependencies(authorizer, request) do
    authorizer
    |> Authorizer.check_context(authorizer_state(request, authorizer))
    |> Enum.filter(fn dependency ->
      match?(%UnresolvedField{}, Map.get(request, dependency))
    end)
  end

  defp strict_check_authorizer(authorizer, request) do
    log(request, "strict checking for #{inspect(authorizer)}")

    authorizer_state = authorizer_state(request, authorizer)

    keys = Authorizer.strict_check_context(authorizer, authorizer_state)

    Authorizer.strict_check(authorizer, authorizer_state, Map.take(request, keys))
  end

  defp check_authorizer(authorizer, request) do
    log(request, "checking for #{inspect(authorizer)}")

    authorizer_state = authorizer_state(request, authorizer)

    keys = Authorizer.check_context(authorizer, authorizer_state)

    Authorizer.check(authorizer, authorizer_state, Map.take(request, keys))
  end

  defp set_authorizer_state(request, authorizer, authorizer_state) do
    %{
      request
      | authorizer_state: Map.put(request.authorizer_state, authorizer, authorizer_state)
    }
  end

  defp authorizer_state(request, authorizer) do
    Map.get(request.authorizer_state, authorizer) || %{}
  end

  def validate_requests(requests) do
    with :ok <- validate_unique_paths(requests),
         :ok <- validate_dependencies(requests) do
      :ok
    else
      {:error, {:impossible, path}} ->
        {:error, ImpossiblePath.exception(impossible_path: path)}

      {:error, paths} ->
        {:error, DuplicatedPath.exception(paths: paths)}
    end
  end

  defp validate_unique_paths(requests) do
    requests
    |> Enum.group_by(& &1.path)
    |> Enum.filter(fn {_path, value} ->
      Enum.count(value, & &1.write_to_data?) > 1
    end)
    |> case do
      [] ->
        :ok

      invalid_paths ->
        invalid_paths = Enum.map(invalid_paths, &elem(&1, 0))

        {:error, invalid_paths}
    end
  end

  defp validate_dependencies(requests) do
    result =
      Enum.reduce_while(requests, :ok, fn request, :ok ->
        case do_build_dependencies(request, requests) do
          :ok -> {:cont, :ok}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)

    case result do
      {:ok, requests} -> {:ok, Enum.reverse(requests)}
      other -> other
    end
  end

  defp do_build_dependencies(request, requests, trail \\ []) do
    request
    |> Map.from_struct()
    |> Enum.reduce_while(:ok, fn
      {_key, %UnresolvedField{deps: deps}}, :ok ->
        case expand_deps(deps, requests, trail) do
          {:error, error} ->
            {:halt, {:error, error}}

          :ok ->
            {:cont, :ok}
        end

      _, :ok ->
        {:cont, :ok}
    end)
  end

  defp expand_deps([], _, _), do: :ok

  defp expand_deps(deps, requests, trail) do
    Enum.reduce_while(deps, :ok, fn dep, :ok ->
      case do_expand_dep(dep, requests, trail) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp do_expand_dep(dep, requests, trail) do
    if dep in trail do
      {:error, {:circular, dep}}
    else
      request_path = :lists.droplast(dep)
      request_key = List.last(dep)

      case Enum.find(requests, &(&1.path == request_path)) do
        nil ->
          {:error, {:impossible, dep}}

        %{^request_key => %UnresolvedField{deps: nested_deps}} ->
          case expand_deps(nested_deps, requests, [dep | trail]) do
            :ok -> :ok
            other -> other
          end

        _ ->
          :ok
      end
    end
  end

  defp log(request, message, level \\ :debug)

  defp log(%{verbose?: true, name: name} = request, message, level) do
    if is_list(request.data) do
      Logger.log(level, "#{name}: #{Enum.count(request.data)} #{message}")
    else
      Logger.log(level, "#{name}: #{message}")
    end
  end

  defp log(_, _, _) do
    false
  end
end
