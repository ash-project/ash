defmodule Ash.Engine.Request do
  @moduledoc """
  Represents an individual request to be processed by the engine.

  See `new/1` for more information
  """
  defstruct [
    :async?,
    :resource,
    :changeset,
    :path,
    :action_type,
    :action,
    :data,
    :notification_data,
    :name,
    :api,
    :query,
    :data_layer_query,
    :authorization_filter,
    :write_to_data?,
    :strict_check_only?,
    :verbose?,
    :state,
    :actor,
    :authorize?,
    :engine_pid,
    :error_path,
    :completion,
    :tracer,
    :trace_prefix,
    async_fetch_state: :not_requested,
    touches_resources: [],
    additional_context: %{},
    notify?: false,
    authorized?: false,
    authorizer_state: %{},
    dependencies_to_send: %{},
    dependency_data: %{}
  ]

  @type t :: %__MODULE__{}

  alias Ash.Authorizer
  alias Ash.Error.Invalid.{DuplicatedPath, ImpossiblePath}

  require Ash.Query
  require Ash.Tracer
  require Logger

  defmodule UnresolvedField do
    @moduledoc """
    Represents an unresolved field to be resolved by the engine.
    """
    defstruct [:resolver, deps: [], data?: false]

    @type t :: %__MODULE__{}

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

  @doc """
  Create an unresolved field.

  Can have dependencies, which is a list of atoms. All elements
  before the last comprise the path of a request that is also
  being processed, like `[:data]`, and the last element is the
  key of that request that is required. Make sure to pass a
  list of lists of atoms. The second argument is a map, which
  contains all of the values you requested, at the same path
  that they were requested.

  For example:

      resolve([[:data, :query], [:data, :data]], fn %{data: %{query: query, data: data}} ->
        data # This is the data field of the [:data] request
        query # This is the query field of the [:data] request

        {:ok, result}
        # or
        {:error, error}
        # or
        result
      end)
  """
  def resolve(dependencies \\ [], func) do
    UnresolvedField.new(Enum.uniq(List.wrap(dependencies)), func)
  end

  @doc """
  Creates a new request.

  The field values may be explicit values, or they may be
  instances of `UnresolvedField`.

  When other requests depend on a value from this request, they will
  not be sent unless this request has completed its authorization (or this
  request has been configured not to do authorization). This allows requests
  to depend on each other without those requests happening just before a request
  fails with a forbidden error. These fields are `data`, `query`, `changeset`
  and `authorized?`.

  A field may not be resolved  if the data of a request has been resolved and
  no other requests depend on that field.

  Options:

    * query - The query to be used to fetch data. Used to authorize reads.
    * data - The ultimate goal of a request is to compute the data
    * resource - The primary resource of the request. Used for opening transactions on creates/updates/destroys
    * changeset - Any changes to be made to the resource. Used to authorize writes.
    * path - The path of the request. This serves as a unique id, and is the way that other requests can refer to this one
    * action_type - The action_type of the request
    * action - The action being performed on the data
    * async? - Whether or not the request *can* be asynchronous, defaults to `true`.
    * api - The api module being called
    * name - A human readable name for the request, used when logging/in errors
    * strict_check_only? - If true, authorization will not be allowed to proceed to a runtime check (so it cannot run db queries unless authorization is assured)
    * actor - The actor performing the action, used for authorization
    * authorize? - Whether or not to perform authorization (defaults to true)
    * verbose? - print informational logs (warning, this will be a whole lot of logs)
    * write_to_data? - If set to false, this value is not returned from the initial call to the engine
  """
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
          raise ArgumentError,
            message: "Unexpected value passed to `Ash.Query.new/1`: #{inspect(other)}"
      end

    data =
      case opts[:data] do
        %UnresolvedField{} = unresolved ->
          %{unresolved | data?: true}

        other ->
          other
      end

    %__MODULE__{
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: List.wrap(opts[:path]),
      action_type: opts[:action_type] || (opts[:action] && opts[:action].type),
      action: opts[:action],
      async?: Keyword.get(opts, :async?, true),
      data: data,
      query: query,
      error_path: opts[:error_path],
      touches_resources: opts[:touches_resources] || [],
      data_layer_query: resolve([], fn _ -> nil end),
      api: opts[:api],
      name: opts[:name],
      strict_check_only?: opts[:strict_check_only?],
      state: :strict_check,
      actor: opts[:actor],
      notify?: opts[:notify?] == true,
      authorize?: Keyword.get(opts, :authorize?, true),
      authorized?: opts[:authorize?] == false,
      verbose?: opts[:verbose?] || false,
      completion: resolve([opts[:path] ++ [:data]], fn _ -> {:ok, :complete} end),
      write_to_data?: Keyword.get(opts, :write_to_data?, true)
    }
  end

  def resource_notification(request) do
    %Ash.Notifier.Notification{
      resource: request.resource,
      api: request.api,
      actor: request.actor,
      action: request.action,
      data: request.notification_data,
      changeset: request.changeset
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
        error = Ash.Error.to_ash_error(error)

        error =
          if request.error_path do
            Ash.Error.set_path(error, request.error_path)
          else
            error
          end

        {:error, error, request}
    end
  end

  def do_next(%{state: :strict_check, authorize?: false} = request) do
    log(request, fn -> "Skipping strict check due to authorize?: false" end)
    {:continue, %{request | state: :fetch_data}, []}
  end

  def do_next(%{state: :strict_check, resource: nil} = request) do
    {:continue, %{request | state: :fetch_data}, []}
  end

  def do_next(%{state: :strict_check} = request) do
    case Ash.Resource.Info.authorizers(request.resource) do
      [] ->
        log(request, fn -> "No authorizers found, skipping strict check" end)
        {:continue, %{request | state: :fetch_data}, []}

      authorizers ->
        case strict_check(authorizers, request) do
          {:ok, new_request, notifications, []} ->
            new_request = set_authorized(new_request)
            log(new_request, fn -> "Strict check complete" end)
            {:continue, %{new_request | state: :fetch_data}, notifications}

          {:ok, new_request, notifications, dependencies} ->
            log(new_request, fn -> "Strict check incomplete, waiting on dependencies" end)
            {:waiting, new_request, notifications, dependencies}

          {:error, error} ->
            log(request, fn -> "Strict checking failed" end)
            {:error, error, request}
        end
    end
  end

  def do_next(%{state: :fetch_data} = request) do
    key =
      case request.changeset do
        %UnresolvedField{} ->
          :changeset

        _ ->
          :data
      end

    case try_resolve_local(request, key, true) do
      {:skipped, new_request, notifications, waiting_for} ->
        {:waiting, new_request, notifications, waiting_for}

      {:ok, request, notifications, []} ->
        if key == :changeset do
          {:continue, request, notifications}
        else
          log(request, fn -> "data fetched: #{inspect(notifications)}" end)
          {:continue, %{request | state: :check}, notifications}
        end

      {:ok, new_request, notifications, waiting_for} ->
        log(request, fn -> "#{key} waiting on dependencies: #{inspect(waiting_for)}" end)
        {:waiting, new_request, notifications, waiting_for}

      {:error, error, new_request} ->
        log(request, fn -> "error fetching #{key}: #{inspect(error)}" end)
        {:error, error, new_request}

      {:error, error} ->
        log(request, fn -> "error fetching #{key}: #{inspect(error)}" end)
        {:error, error, request}
    end
  end

  def do_next(%{state: :check, authorize?: false} = request) do
    log(request, fn -> "Skipping check due to `authorize?: false`" end)
    {:complete, %{request | state: :complete}, [], []}
  end

  def do_next(%{state: :check, resource: nil} = request) do
    {:complete, %{request | state: :complete}, [], []}
  end

  def do_next(%{state: :check} = request) do
    case Ash.Resource.Info.authorizers(request.resource) do
      [] ->
        log(request, fn -> "No authorizers found, skipping check" end)
        {:complete, %{request | state: :complete}, [], []}

      authorizers ->
        case check(authorizers, request) do
          {:ok, new_request, notifications, []} ->
            log(new_request, fn -> "Check complete" end)
            new_request = set_authorized(new_request)

            {:complete, %{new_request | state: :complete}, notifications, []}

          {:ok, new_request, notifications, waiting} ->
            log(request, fn -> "Check incomplete, waiting on dependencies" end)
            {:waiting, new_request, notifications, waiting}

          {:error, error} ->
            log(request, fn -> "Check failed" end)
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
              new_request = %{new_request | state: :complete}

              {:cont,
               {:complete, new_request, new_notifications ++ notifications, other_deps ++ deps}}

            {:ok, new_request, new_notifications, other_deps} ->
              new_request = %{new_request | state: :complete}

              {:cont,
               {:complete, new_request, new_notifications ++ notifications, other_deps ++ deps}}

            {:error, error} ->
              {:halt, {:error, error, request}}
          end
      end)
    end
  end

  def summarize(%{path: path, name: name, action: %{name: action}, resource: resource})
      when not is_nil(resource) do
    "#{inspect(path, structs: false)}\n#{name}: #{inspect(resource)}.#{action}"
  end

  def summarize(%{path: path, name: name}) do
    "#{inspect(path, structs: false)} - #{name}"
  end

  def sort_and_clean_notifications(notifications) do
    {front, back} =
      notifications
      |> List.wrap()
      |> Enum.uniq()
      |> Enum.split_with(fn
        {:requests, _} ->
          true

        _ ->
          false
      end)

    front ++ back
  end

  def wont_receive(request, path, field) do
    log(request, fn -> "Request failed due to failed dependency #{inspect(path ++ [field])}" end)

    {:stop, :dependency_failed, request}
  end

  def send_field(request, receiver_path, field) do
    log(request, fn -> "Attempting to provide #{inspect(field)} for #{inspect(receiver_path)}" end)

    case store_dependency(request, receiver_path, field) do
      {:ok, new_request, notifications} ->
        {:ok, new_request, notifications}

      {:waiting, new_request, notifications, waiting_for} ->
        {:waiting, new_request, notifications, waiting_for}

      {:error, error, new_request} ->
        log(request, fn -> "Error resolving #{field}: #{inspect(error)}" end)
        error = Ash.Error.to_ash_error(error)

        error =
          if request.error_path do
            Ash.Error.set_path(error, request.error_path)
          else
            error
          end

        {:error, error, new_request}
    end
  end

  def receive_field(request, path, field, value) do
    log(request, fn -> "Receiving field #{field} from #{inspect(path)}" end)

    new_request = put_dependency_data(request, path ++ [field], value)

    {:continue, new_request}
  end

  defp set_authorized(%{authorized?: false, resource: resource} = request) do
    authorized? =
      resource
      |> Ash.Resource.Info.authorizers()
      |> Enum.all?(fn authorizer ->
        authorizer_state(request, authorizer) == :authorized
      end)

    %{request | authorized?: authorized?}
  end

  defp set_authorized(request), do: request

  def put_dependency_data(request, dep, value) do
    %{request | dependency_data: Map.put(request.dependency_data, dep, value)}
  end

  def store_dependency(error, receiver_path, field, internal? \\ false)

  def store_dependency(%{state: :error} = request, _receiver_path, _field, _internal?) do
    {:ok, request, []}
  end

  def store_dependency(request, receiver_path, field, internal?) do
    request = do_store_dependency(request, field, receiver_path)

    case try_resolve_local(request, field, internal?) do
      {:skipped, new_request, notifications, []} ->
        log(request, fn -> "Field #{field} was skipped, no additional dependencies" end)
        {:ok, new_request, notifications}

      {:skipped, new_request, notifications, waiting} ->
        log(request, fn ->
          "Field #{field} was skipped, registering dependencies: #{inspect(waiting)}"
        end)

        {:waiting, new_request, notifications, waiting}

      {:ok, new_request, notifications, []} ->
        case Map.get(new_request, field) do
          %UnresolvedField{} ->
            log(request, fn -> "Field could not be resolved #{field}, registering dependency" end)

            {:ok, new_request, notifications}

          value ->
            log(request, fn -> "Field #{field}, was resolved and provided" end)
            {new_request, new_notifications} = notifications(new_request, field, value)

            {:ok, new_request, new_notifications ++ notifications}
        end

      {:ok, new_request, notifications, waiting} ->
        case Map.get(new_request, field) do
          %UnresolvedField{} ->
            log(request, fn -> "Field could not be resolved #{field}, registering dependency" end)

            {:waiting, new_request, notifications, waiting}

          value ->
            log(request, fn -> "Field #{field}, was resolved and provided" end)
            {new_request, new_notifications} = notifications(new_request, field, value)

            {:ok, new_request, new_notifications ++ notifications, waiting}
        end

      {:error, error} ->
        {:error, error, request}
    end
  end

  defp do_store_dependency(request, field, receiver_path) do
    log(request, fn -> "storing dependency on #{field} from #{inspect(receiver_path)}" end)

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
      log(request, fn -> "strict checking" end)

      case do_strict_check(authorizer, request) do
        {:ok, new_request} ->
          log(new_request, fn -> "strict check succeeded for #{inspect(authorizer)}" end)
          {:cont, {:ok, new_request, notifications, waiting_for}}

        {:ok, new_request, new_notifications, new_deps} ->
          log(new_request, fn -> "strict check succeeded for #{inspect(authorizer)}" end)
          {:cont, {:ok, new_request, new_notifications ++ notifications, waiting_for ++ new_deps}}

        {:waiting, new_request, new_notifications, new_deps} ->
          log(
            new_request,
            fn -> "waiting on dependencies: #{inspect(new_deps)} for #{inspect(authorizer)}" end
          )

          {:cont, {:ok, new_request, notifications ++ new_notifications, new_deps ++ waiting_for}}

        {:error, error} ->
          log(request, fn ->
            "strict check failed for #{inspect(authorizer)}: #{inspect(error)}"
          end)

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_strict_check(authorizer, request, notifications \\ []) do
    strict_check_only? = request.strict_check_only?

    case missing_strict_check_dependencies?(authorizer, request) do
      [] ->
        case strict_check_authorizer(authorizer, request) do
          {:authorized, _authorizer} ->
            {:ok, set_authorizer_state(request, authorizer, :authorized), notifications, []}

          {:filter, authorizer_state, filter} ->
            request
            |> set_authorizer_state(authorizer, authorizer_state)
            |> apply_filter(authorizer, filter, true)
            |> case do
              {:ok, request} ->
                {:ok, request, notifications, []}

              {:ok, request, new_notifications, deps} ->
                {:ok, request, new_notifications ++ notifications, deps}

              other ->
                other
            end

          {:filter, filter} ->
            request
            |> apply_filter(authorizer, filter, true)
            |> case do
              {:ok, request} ->
                {:ok, request, notifications, []}

              {:ok, request, new_notifications, deps} ->
                {:ok, request, new_notifications ++ notifications, deps}

              other ->
                other
            end

          {:filter_and_continue, _, authorizer_state} when strict_check_only? ->
            {:error,
             Authorizer.exception(
               authorizer,
               :must_pass_strict_check,
               authorizer_state
             )}

          {:filter_and_continue, filter, new_authorizer_state} ->
            request
            |> set_authorizer_state(authorizer, new_authorizer_state)
            |> apply_filter(authorizer, filter)
            |> case do
              {:ok, request} ->
                {:ok, request, notifications, []}

              {:ok, request, new_notifications, deps} ->
                {:ok, request, new_notifications ++ notifications, deps}

              other ->
                other
            end

          {:continue, authorizer_state} when strict_check_only? ->
            {:error,
             Authorizer.exception(
               authorizer,
               :must_pass_strict_check,
               authorizer_state
             )}

          {:continue, authorizer_state} ->
            {:ok, set_authorizer_state(request, authorizer, authorizer_state), notifications, []}

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

  defp apply_filter(request, authorizer, filter, resolve_data? \\ false)

  defp apply_filter(
         %{action: %{type: :read}} = request,
         authorizer,
         filter,
         resolve_data?
       ) do
    request =
      request
      |> Map.update!(:query, &Ash.Query.filter(&1, ^filter))
      |> Map.update(
        :authorization_filter,
        filter,
        &add_to_or_parse(&1, filter, request.resource, request.query)
      )
      |> set_authorizer_state(authorizer, :authorized)

    if resolve_data? do
      try_resolve(request, [request.path ++ [:query]], false)
    else
      {:ok, request}
    end
  end

  defp apply_filter(request, authorizer, filter, resolve_data?) do
    case do_runtime_filter(request, filter, authorizer, get_authorizer_state(request, authorizer)) do
      {:ok, request} ->
        request = set_authorizer_state(request, authorizer, :authorized)

        if resolve_data? do
          try_resolve(request, [request.path ++ [:query]], false)
        else
          {:ok, request}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_to_or_parse(existing_authorization_filter, filter, resource, query) do
    if existing_authorization_filter do
      Ash.Filter.add_to_filter(
        existing_authorization_filter,
        filter,
        query.aggregates,
        query.calculations,
        query.context
      )
    else
      Ash.Filter.parse!(resource, filter, query.aggregates, query.calculations, query.context)
    end
  end

  defp check(authorizers, request) do
    authorizers
    |> Enum.reject(&(authorizer_state(request, &1) == :authorized))
    |> Enum.reduce_while({:ok, request, [], []}, fn authorizer,
                                                    {:ok, request, notifications, waiting_for} ->
      case do_check(authorizer, request) do
        {:ok, new_request} ->
          log(request, fn -> "check succeeded for #{inspect(authorizer)}" end)
          {:cont, {:ok, new_request, notifications, waiting_for}}

        {:ok, new_request, new_notifications, new_deps} ->
          log(request, fn -> "check succeeded for #{inspect(authorizer)}" end)
          {:cont, {:ok, new_request, new_notifications ++ notifications, new_deps ++ waiting_for}}

        {:waiting, new_request, new_notifications, new_deps} ->
          log(
            request,
            fn -> "waiting on dependencies: #{inspect(new_deps)} for #{inspect(authorizer)}" end
          )

          {:cont, {:ok, new_request, new_notifications ++ notifications, new_deps ++ waiting_for}}

        {:error, error} ->
          log(request, fn -> "check failed for #{inspect(authorizer)}: #{inspect(error)}" end)

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
    case do_runtime_filter(request, filter, authorizer, get_authorizer_state(request, authorizer)) do
      {:ok, request} ->
        request
        |> set_authorizer_state(authorizer, :authorized)
        |> try_resolve([request.path ++ [:data], request.path ++ [:query]], false)

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_runtime_filter(
         request,
         nil,
         _authorizer,
         _authorizer_state
       ),
       do: {:ok, request}

  defp do_runtime_filter(
         %{action: %{type: :read}, data: empty} = request,
         _filter,
         _authorizer,
         _authorizer_state
       )
       when empty in [nil, []],
       do: {:ok, request}

  defp do_runtime_filter(
         %{action: %{type: :read}} = request,
         filter,
         _authorizer,
         _authorizer_state
       ) do
    pkey = Ash.Resource.Info.primary_key(request.resource)

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
      |> Ash.Query.filter(^primary_key_filter)
      |> Ash.Query.filter(^filter)

    new_query
    |> Map.put(:api, request.api)
    |> Ash.Query.select([])
    |> Ash.Query.data_layer_query()
    |> case do
      {:ok, data_layer_query} ->
        data_layer_query
        |> Ash.DataLayer.run_query(request.resource)
        |> case do
          {:ok, results} ->
            pkey = Ash.Resource.Info.primary_key(request.resource)
            pkeys = Enum.map(results, &Map.take(&1, pkey))

            new_data = Enum.filter(request.data, &(Map.take(&1, pkey) in pkeys))

            {:ok, %{request | data: new_data, query: new_query}}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_runtime_filter(
         %{action: %{type: :create}},
         _filter,
         _authorizer,
         _authorizer_state
       ) do
    {:error, Ash.Error.Forbidden.CannotFilterCreates.exception([])}
  end

  defp do_runtime_filter(request, filter, authorizer, authorizer_state) do
    pkey = Ash.Resource.Info.primary_key(request.resource)

    pkey =
      request.changeset.data
      |> Map.take(pkey)
      |> Map.to_list()

    new_query =
      request.resource
      |> Ash.Query.set_tenant(request.changeset.tenant)
      |> Ash.Query.set_context(request.changeset.context)
      |> Ash.Query.filter(^filter)
      |> Ash.Query.limit(1)

    query_with_pkey_filter = Ash.Query.filter(new_query, ^pkey)

    query_with_pkey_filter
    |> Map.put(:api, request.api)
    |> Ash.Query.select([])
    |> Ash.Query.data_layer_query()
    |> case do
      {:ok, data_layer_query} ->
        data_layer_query
        |> Ash.DataLayer.run_query(request.resource)
        |> case do
          {:ok, []} ->
            {:error,
             Authorizer.exception(
               authorizer,
               {:changeset_doesnt_match_filter, new_query.filter},
               authorizer_state
             )}

          {:ok, [_]} ->
            {:ok, request}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  rescue
    e ->
      Logger.error(
        "Exception while running authorization query: #{inspect(Exception.format(:error, e, __STACKTRACE__))}"
      )

      {:error,
       Authorizer.exception(
         authorizer,
         :unknown,
         authorizer_state
       )}
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

    # Don't honor requests for special fields until the request is authorized
    if field in [
         :data,
         :query,
         :changeset,
         :authorized?,
         :data_layer_query,
         :authorization_filter
       ] and not authorized? and
         not internal? do
      try_resolve_dependencies_of(request, field, internal?)
    else
      case Map.get(request, field) do
        %UnresolvedField{} = unresolved ->
          do_try_resolve_local(request, field, unresolved, internal?)

        value ->
          notify_existing_value(request, field, value)
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

  defp notify_existing_value(request, field, value) do
    {new_request, notifications} = notifications(request, field, value)

    {:ok, new_request, notifications, []}
  end

  defp do_try_resolve_local(request, field, unresolved, internal?) do
    %{deps: deps, resolver: resolver} = unresolved

    with {:ok, new_request, notifications, []} <-
           try_resolve(request, deps, internal?) do
      resolver_context = resolver_context(new_request, deps)

      log(request, fn -> "resolving #{field}" end)

      # Currently, only the `data` field is resolved asynchronously

      cond do
        field == :completion && new_request.state != :complete ->
          {:skipped, new_request, notifications, []}

        field == :data && new_request.async? &&
            !match?({:fetched, _}, new_request.async_fetch_state) ->
          if new_request.async_fetch_state in [:requested, :fetching] do
            {:skipped, new_request, notifications, []}
          else
            {:skipped, %{new_request | async_fetch_state: {:requested, resolver_context}},
             notifications, []}
          end

        true ->
          # Here we reset async_fetch_state to `:not_requested`
          # because one of two things is true:
          # the resolver returned additional requests, in which case
          # we will need to call it again later, *or* the resolution
          # moves the request onto the next step in which case
          # async_fetch_state is no longer in play.
          {new_request, result} =
            if field == :data && new_request.async? do
              {_, result} = new_request.async_fetch_state
              {%{new_request | async_fetch_state: :not_requested}, result}
            else
              if field == :data do
                Ash.Tracer.span :request_step,
                                request.name,
                                request.tracer do
                  metadata = %{
                    resource_short_name:
                      request.resource && Ash.Resource.Info.short_name(request.resource),
                    resource: request.resource,
                    actor: request.actor,
                    action: request.action && request.action.name,
                    authorize?: request.authorize?
                  }

                  Ash.Tracer.set_metadata(request.tracer, :request_step, metadata)

                  Ash.Tracer.telemetry_span [:ash, :request_step], %{name: request.name} do
                    {%{new_request | async_fetch_state: :not_requested},
                     resolver.(resolver_context)}
                  end
                end
              else
                {%{new_request | async_fetch_state: :not_requested}, resolver.(resolver_context)}
              end
            end

          result =
            case result do
              {:requests, requests} ->
                {:requests, requests, []}

              other ->
                other
            end

          case result do
            {:new_deps, new_deps} ->
              log(request, fn -> "New dependencies for #{field}: #{inspect(new_deps)}" end)

              new_request =
                Map.put(
                  new_request,
                  field,
                  %{unresolved | deps: Enum.uniq(unresolved.deps ++ new_deps)}
                )

              {:skipped, new_request, notifications, new_deps}

            {:requests, requests, new_deps} ->
              log(request, fn ->
                paths =
                  Enum.map(requests, fn
                    {request, _} ->
                      request.path

                    request ->
                      request.path
                  end)

                "#{field} added requests #{inspect(paths)}"
              end)

              new_deps =
                requests
                |> Enum.flat_map(fn
                  {request, key} ->
                    [request.path ++ [key]]

                  _request ->
                    []
                end)
                |> Enum.concat(new_deps)

              new_unresolved =
                Map.update!(
                  unresolved,
                  :deps,
                  &Enum.uniq(&1 ++ new_deps)
                )

              new_request = Map.put(new_request, field, new_unresolved)

              new_requests =
                Enum.map(requests, fn
                  {request, _} ->
                    %{request | error_path: request.error_path || new_request.error_path}

                  request ->
                    %{request | error_path: request.error_path || new_request.error_path}
                end)

              {:skipped, new_request,
               notifications ++
                 [{:requests, new_requests}], new_deps}

            {:ok, value, instructions} ->
              log(request, fn ->
                "successfully resolved #{field} with instructions"
              end)

              set_data_notifications =
                Enum.map(Map.get(instructions, :extra_data, %{}), fn {key, value} ->
                  {:set_extra_data, key, value}
                end)

              new_request = Map.merge(new_request, instructions[:set_keys] || %{})

              resource_notifications = Map.get(instructions, :notifications, [])

              extra_requests = Map.get(instructions, :requests, [])

              unless Enum.empty?(extra_requests) do
                log(new_request, fn ->
                  "added requests #{inspect(Enum.map(extra_requests, & &1.path))}"
                end)
              end

              request_notifications =
                case extra_requests do
                  [] ->
                    []

                  nil ->
                    []

                  requests ->
                    [
                      {:requests,
                       Enum.map(requests, fn
                         {request, _} ->
                           %{request | error_path: request.error_path || new_request.error_path}

                         request ->
                           %{request | error_path: request.error_path || new_request.error_path}
                       end)}
                    ]
                end

              handle_successful_resolve(
                field,
                value,
                new_request,
                notifications ++
                  resource_notifications ++
                  set_data_notifications ++ request_notifications
              )

            {:ok, value} ->
              log(request, fn ->
                "successfully resolved #{field}"
              end)

              handle_successful_resolve(
                field,
                value,
                new_request,
                notifications
              )

            {:error, error, %{set: %{changeset: new_changeset}}} ->
              {:error, error, %{request | changeset: new_changeset}}

            {:error, error, %{set: %{query: new_query}}} ->
              {:error, error, %{request | query: new_query}}

            {:error, error} ->
              log(request, fn ->
                "error resolving #{field}:\n #{inspect(error)}"
              end)

              {:error, error}
          end
      end
    end
  rescue
    e ->
      reraise Ash.Error.to_ash_error(e, __STACKTRACE__,
                error_context: "resolving #{field} on #{request.name}"
              ),
              __STACKTRACE__
  end

  defp handle_successful_resolve(field, value, new_request, notifications) do
    value = process_resolved_field(field, value, new_request)

    {new_request, new_notifications} = notifications(new_request, field, value)

    notifications =
      Enum.concat([
        notifications,
        new_notifications
      ])

    new_request = Map.put(new_request, field, value)

    {:ok, new_request, notifications, []}
  end

  defp process_resolved_field(:query, %Ash.Query{} = query, request) do
    Ash.Query.set_context(query, %{
      authorize?: request.authorize?,
      actor: request.actor
    })
  end

  defp process_resolved_field(:changeset, %Ash.Changeset{} = changeset, request) do
    Ash.Changeset.set_context(changeset, %{
      authorize?: request.authorize?,
      actor: request.actor
    })
  end

  defp process_resolved_field(_, value, _), do: value

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
    |> Map.put(:verbose?, request.verbose?)
    |> Map.put(:actor, request.actor)
    |> Map.put(:authorize?, request.authorize?)
    |> Map.put(:async?, request.async?)
    |> Ash.Helpers.deep_merge_maps(request.additional_context || %{})
  end

  defp local_dep?(request, dep) do
    :lists.droplast(dep) == request.path
  end

  def add_initial_authorizer_state(%{resource: nil} = request), do: request

  def add_initial_authorizer_state(request) do
    request.resource
    |> Ash.Resource.Info.authorizers()
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
    |> List.wrap()
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
    log(request, fn -> "strict checking for #{inspect(authorizer)}" end)

    authorizer_state = authorizer_state(request, authorizer)

    keys = Authorizer.strict_check_context(authorizer, authorizer_state)

    Authorizer.strict_check(
      authorizer,
      authorizer_state,
      Map.take(request, keys)
    )
  end

  defp check_authorizer(authorizer, request) do
    log(request, fn -> "checking for #{inspect(authorizer)}" end)

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

  defp get_authorizer_state(request, authorizer) do
    request.authorizer_state[authorizer] || %{}
  end

  defp authorizer_state(request, authorizer) do
    Map.get(request.authorizer_state, authorizer) || %{}
  end

  def validate_requests!(requests) do
    validate_unique_paths!(requests)
    validate_dependencies!(requests)
    :ok
  end

  defp validate_unique_paths!(requests) do
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

        raise DuplicatedPath, paths: invalid_paths
    end
  end

  defp validate_dependencies!(requests) do
    Enum.each(requests, &do_build_dependencies(&1, requests))
    :ok
  end

  defp do_build_dependencies(request, requests, trail \\ []) do
    request
    |> Map.from_struct()
    |> Enum.each(fn
      {_key, %UnresolvedField{deps: deps}} ->
        expand_deps(deps, requests, trail)

      _ ->
        :ok
    end)
  end

  defp expand_deps(deps, requests, trail) do
    case do_expand_deps(deps, requests, trail) do
      :ok ->
        :ok

      {:error, {:impossible, dep}} ->
        raise ImpossiblePath, impossible_path: dep, paths: Enum.map(requests, & &1.path)
    end
  end

  defp do_expand_deps([], _, _), do: :ok

  defp do_expand_deps(deps, requests, trail) do
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
          case do_expand_deps(nested_deps, requests, [dep | trail]) do
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
      Logger.log(level, fn ->
        message = message.()

        "#{name}: #{Enum.count(request.data)} #{message}"
      end)
    else
      Logger.log(level, fn ->
        message = message.()

        "#{name}: #{message}"
      end)
    end
  end

  defp log(_, _, _) do
    false
  end
end
