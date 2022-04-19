defmodule Ash.Engine do
  @moduledoc """
  The Ash engine handles the parallelization/running of requests to Ash.

  Much of the complexity of this doesn't come into play for simple requests.
  The way it works is that it accepts a list of `Ash.Engine.Request` structs.
  Some of values on those structs will be instances of `Ash.Engine.Request.UnresolvedField`.
  These unresolved fields can express a dependence on the field values from other requests.
  This allows the engine to wait on executing some code until it has its required inputs,
  or if all of its dependencies are met, it can execute it immediately. The engine's job is
  to resolve its unresolved fields in the proper order, potentially in parallel.
  It also has knowledge baked in about certain special fields, like `data` which is the
  field we are ultimately trying to resolve, and `query` which is the field that drives authorization
  for read requests. Authorization is done on a *per engine request* basis.

  As the complexity of a system grows, it becomes very difficult to write code that
  is both imperative and performant. This is especially true of a framework that is
  designed to be configurable. What exactly is done, as well as the order it is done in,
  and wether or not is can be parallelized, varies wildly based on factors like how
  the resources are configured and what capabilities the data layer has. By implementing
  a generic "parallel engine", we can let the engine solve that problem. We simply
  have to express the various operations that must happen, and what other pieces of data
  they need in order to happen, and the engine handles the rest.

  There are various tradeoffs in the current design. The original version of the engine started a process
  for each request. While this had the least constrained performance characteristics of all the designs,
  it was problematic for various reasons. The primary reason being that it could deadlock without any
  reasonable way to debug said deadlock because the various states were distributed. The second version
  of the engine introduced a central `Engine` process that helped with some of these issues, but ultimately
  had the same problem. The third (and current) version of the engine is reworked instead to be drastically
  simpler, potentially at the expense of performance for some requests. Instead of starting a process per
  request, it opts to only parallelize the `data` field resolution of fields that are marked as `async?: true`,
  (unlike the previous versions which started a process for the whole request.) Although it does its best
  to prioritize starting any async tasks, it is possible that if some mix of async/sync requests are passed in
  a potentially long running sync task could prevent it from starting an async task, giving this potentially worse
  performance characteristics. In practice, this doesn't really matter because the robust data layers support running
  asynchronously, unless they are in a transaction in which case everything runs serially anyway.

  The current version of the engine can be seen as an event loop that will async some events and yield them. It also
  has support for a concurrency limit (per engine invocation, not globally, although that could now be added much more
  easily). This limit defaults to `2 * schedulers_online`.

  Check out the docs for `Ash.Engine.Request` for some more information. This is a private
  interface at the moment, though, so this documentation is just here to explain how it works
  it is not intended to give you enough information to use the engine directly.
  """

  defstruct [
    :ref,
    :id,
    :resolved_fields,
    :authorize?,
    :actor,
    :verbose?,
    :async?,
    :concurrency_limit,
    # There are no other failure modes, but this is there
    # to express the intent for there to eventually be.
    failure_mode: :complete,
    opts: [],
    requests: [],
    data: %{},
    unsent_dependencies: [],
    dependencies_seen: MapSet.new(),
    dependencies: %{},
    reverse_dependencies: %{},
    resource_notifications: [],
    tasks: [],
    errors: [],
    notifications: MapSet.new(),
    pending_tasks: []
  ]

  alias Ash.Engine.Request
  require Logger

  def run(requests, opts \\ []) do
    cond do
      opts[:transaction?] && !opts[:resource] ->
        raise "Engine invoked with `transaction?: true` but no resource, so no transaction could be started."

      opts[:transaction?] && Ash.DataLayer.data_layer_can?(opts[:resource], :transact) ->
        Ash.DataLayer.transaction(
          opts[:resource],
          fn ->
            case do_run(requests, opts) do
              {:ok, result} ->
                result

              {:error, error} ->
                Ash.DataLayer.rollback(opts[:resource], error)
            end
          end,
          opts[:timeout] || :infinity
        )

      true ->
        do_run(requests, opts)
    end
  end

  def do_run(requests, opts \\ []) do
    state =
      %__MODULE__{
        ref: make_ref(),
        id: System.unique_integer([:positive, :monotonic]),
        resolved_fields: %{},
        actor: opts[:actor],
        concurrency_limit: System.schedulers_online() * 2,
        authorize?: opts[:authorize?] || false,
        verbose?: opts[:verbose?] || false,
        async?: !Application.get_env(:ash, :disable_async?),
        opts: opts
      }
      |> add_requests(requests)

    log(state, fn ->
      "Engine Starting - #{Enum.map_join(state.requests, ", ", & &1.name)}"
    end)

    case run_to_completion(state) do
      %__MODULE__{errors: [], resource_notifications: resource_notifications} = result ->
        unsent = Ash.Notifier.notify(resource_notifications)

        {:ok,
         result.requests
         |> Enum.reduce(result, &add_data(&2, &1.path, &1.data))
         |> Map.put(:resource_notifications, unsent)}

      state ->
        {:error, state}
    end
  end

  defp add_data(state, path, data) do
    %{
      state
      | data: put_nested_key(state.data, path, data)
    }
  end

  defp set_async(requests) do
    if Application.get_env(:ash, :disable_async?) do
      Enum.map(requests, &Map.put(&1, :async?, false))
    else
      Enum.map(requests, fn request ->
        if must_be_local?(request) do
          %{request | async?: false}
        else
          request
        end
      end)
    end
  end

  @doc false
  def must_be_local?(%{async?: false}), do: true

  def must_be_local?(request) do
    [request.resource | request.touches_resources || []]
    |> Enum.filter(& &1)
    |> Enum.any?(fn resource ->
      (Ash.DataLayer.data_layer_can?(resource, :transact) &&
         Ash.DataLayer.in_transaction?(resource)) ||
        not Ash.DataLayer.data_layer_can?(resource, :async_engine)
    end)
  end

  defp run_to_completion(state) do
    if Enum.all?(state.requests, &(&1.state in [:complete, :error])) do
      log(state, "Engine Complete")
      state
    else
      state
      |> run_iteration()
      |> case do
        ^state ->
          if state.tasks == [] && state.pending_tasks == [] do
            detect_deadlocks(state)

            raise """
            Engine Deadlock! No async tasks and state is the same after iteration.
            #{long_breakdown(state)}
            """
          else
            state
            |> run_iteration()
            |> run_to_completion()
          end

        new_state ->
          run_to_completion(new_state)
      end
    end
  end

  defp errors(state) do
    if state.errors == [] do
      ""
    else
      """
      Errors:
      #{Enum.map_join(state.errors, "\n", fn error -> Exception.format(:error, Ash.Error.to_ash_error(error), stacktrace(error)) end)}
      """
    end
  end

  defp stacktrace(%{stacktraces?: true, stacktrace: %{stacktrace: stacktrace}})
       when not is_nil(stacktrace) do
    stacktrace
  end

  defp stacktrace(_), do: nil

  defp completed_requests(state) do
    state.requests
    |> Enum.filter(&(&1.state == :complete))
    |> case do
      [] ->
        ""

      requests ->
        """
        Complete Requests:
        #{Enum.map_join(requests, "\n\n", fn request -> Request.summarize(request) end)}
        """
    end
  end

  defp errored_requests(state) do
    state.requests
    |> Enum.filter(&(&1.state == :error))
    |> case do
      [] ->
        ""

      requests ->
        """
        Errored Requests:
        #{Enum.map_join(requests, "\n\n", fn request -> Request.summarize(request) end)}
        """
    end
  end

  defp pending_requests(state) do
    state.requests
    |> Enum.reject(&(&1.state in [:complete, :error]))
    |> case do
      [] ->
        ""

      requests ->
        """
        Requests:
        #{Enum.map_join(requests, "\n\n", fn request -> Request.summarize(request) <> "\n  " <> depends_on_summary(request, state) end)}
        """
    end
  end

  defp depends_on_summary(request, state) do
    dependencies = state.dependencies[request.path] || []

    if Enum.empty?(dependencies) do
      " state: #{request.state}"
    else
      " state: #{request.state} | depends on #{Enum.map_join(dependencies, ", ", &name_of(&1, state))}"
    end
  end

  defp name_of({path, dep}, state) do
    "#{inspect(Enum.find(state.requests, &(&1.path == path)).name)}.#{dep}"
  end

  defp run_iteration(%__MODULE__{tasks: tasks} = state) when tasks != [] do
    Enum.reduce(tasks, state, fn task, state ->
      case Task.yield(task, 0) do
        {:ok, {request_path, result}} ->
          state = %{state | tasks: tasks -- [task]}
          request = Enum.find(state.requests, &(&1.path == request_path))

          new_request = %{request | async_fetch_state: {:fetched, result}}

          replace_request(state, new_request)

        nil ->
          state
      end
    end)
  end

  defp run_iteration(
         %__MODULE__{unsent_dependencies: [{request_path, dep} | remaining_unsent_dependencies]} =
           state
       ) do
    state = %{state | unsent_dependencies: remaining_unsent_dependencies}
    request = Enum.find(state.requests, &(&1.path == request_path))
    path = :lists.droplast(dep)
    field = List.last(dep)

    depended_on_request = Enum.find(state.requests, &(&1.path == path))

    cond do
      request.state == :error ->
        state

      depended_on_request.state == :error ->
        case Request.wont_receive(request, path, field) do
          {:stop, :dependency_failed, new_request} ->
            new_request = %{new_request | state: :error}

            state
            |> replace_request(new_request)
            |> notify_error(new_request)
        end

      true ->
        {state, notifications, dependencies} =
          notify_local_request(
            state,
            depended_on_request,
            request,
            field
          )

        state
        |> notify(notifications)
        |> store_dependencies(dependencies)
    end
  end

  defp run_iteration(state) do
    state = start_pending_tasks(state)

    case Enum.find(state.requests, &match?({:requested, _}, &1.async_fetch_state)) do
      nil ->
        {async, sync} =
          state.requests
          |> Enum.filter(fn request ->
            Enum.empty?(state.dependencies[request.path] || [])
          end)
          |> Enum.split_with(& &1.async?)

        {state, do_sync?} =
          Enum.reduce(async, {state, true}, fn
            request, {state, false} ->
              {do_run_iteration(state, request), false}

            request, {state, true} ->
              new_state = do_run_iteration(state, request)
              # We only want to process synchronous requests once all asynchronous requests
              # have done all of their work.
              {new_state, state == new_state}
          end)

        if do_sync? do
          Enum.reduce(sync, state, &do_run_iteration(&2, &1))
        else
          state
        end

      request ->
        {_, resolver_context} = request.async_fetch_state
        request = %{request | async_fetch_state: :fetching}

        if Enum.count(state.tasks) >= state.concurrency_limit do
          pending_task = fn ->
            {request.path, request.data.resolver.(resolver_context)}
          end

          %{state | pending_tasks: [pending_task | state.pending_tasks]}
        else
          task =
            Task.async(fn ->
              {request.path, request.data.resolver.(resolver_context)}
            end)

          %{state | tasks: [task | state.tasks]}
        end
    end
  end

  defp do_run_iteration(state, request) do
    log(state, fn -> breakdown(state) end)
    {state, notifications, dependencies} = fully_advance_request(state, request)

    state
    |> notify(notifications)
    |> store_dependencies(dependencies)
  end

  defp detect_deadlocks(state) do
    state.dependencies
    |> Enum.each(fn {path, deps} ->
      case Enum.find_value(deps, &depends_on(state, &1, path)) do
        nil ->
          :ok

        {circular_dep, path} ->
          raise "Deadlock detected! #{inspect(path)} and #{circular_dep} depend on each other via #{Enum.map_join(path, " -> ", &inspect/1)}"

        circular_dep ->
          raise "Deadlock detected! #{inspect(path)} and #{circular_dep} depend on each other"
      end
    end)

    state
  end

  defp depends_on(state, source, destination, trail \\ []) do
    deps =
      state.dependencies[source]
      |> Kernel.||([])

    if Enum.any?(deps, fn {path, _} -> path == destination end) do
      source
    else
      deps
      |> Enum.reject(&(&1 in trail))
      |> Enum.find(&depends_on(state, &1, destination, [&1 | []]))
      |> case do
        nil ->
          false

        {circular_dep, path} ->
          {source, path ++ circular_dep}

        circular_dep ->
          {source, [circular_dep]}
      end
    end
  end

  defp breakdown(state) do
    """
    State breakdown:
    #{Enum.map_join(state.requests, "\n", &"#{&1.name}: #{&1.state}")}
    """
  end

  defp long_breakdown(state) do
    """
    #{errors(state)}
    #{completed_requests(state)}
    #{errored_requests(state)}
    #{pending_requests(state)}
    """
  end

  defp fully_advance_request(state, request) do
    case advance_request(request) do
      {:ok, new_request, notifications, dependencies, resource_notification} ->
        new_state =
          state
          |> replace_request(new_request)
          |> add_resource_notification(resource_notification)

        new_dependencies = build_dependencies(new_request, dependencies)

        {new_state, notifications, new_dependencies}

      {:ok, new_request, notifications, dependencies} ->
        new_state = replace_request(state, new_request)

        new_dependencies = build_dependencies(new_request, dependencies)

        {new_state, notifications, new_dependencies}

      {:error, error, new_request} ->
        new_request = %{new_request | state: :error}

        new_state =
          state
          |> add_error(new_request.path, error)
          |> replace_request(new_request)
          |> notify_error(new_request)

        {new_state, [], []}
    end
  end

  defp notify_error(state, new_request) do
    state.reverse_dependencies[new_request.path]
    |> Kernel.||([])
    |> Enum.reduce(state, fn {request_path_that_wont_receive, dep_they_wont_receive}, state ->
      request = Enum.find(state.requests, &(&1.path == request_path_that_wont_receive))

      if request.state == :error do
        state
      else
        case Request.wont_receive(
               request,
               new_request.path,
               dep_they_wont_receive
             ) do
          {:stop, :dependency_failed, new_request} ->
            new_request = %{new_request | state: :error}

            state
            |> notify_error(new_request)
            |> replace_request(new_request)
        end
      end
    end)
  end

  defp start_pending_tasks(%{pending_tasks: []} = state), do: state

  defp start_pending_tasks(state) do
    available_tasks = state.concurrency_limit - Enum.count(state.tasks)

    {to_start, remaining} = Enum.split(state.pending_tasks, available_tasks)

    state = %{state | pending_tasks: remaining}

    new_tasks = Enum.map(to_start, &Task.async/1)

    %{state | tasks: state.tasks ++ new_tasks}
  end

  defp advance_request(%{state: state} = request) when state in [:error] do
    {:ok, request, [], []}
  end

  defp advance_request(request) do
    case Request.next(request) do
      {:already_complete, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications, new_dependencies}

      {:complete, new_request, new_notifications, new_dependencies} ->
        if new_request.resource && new_request.notify? do
          resource_notification = Request.resource_notification(new_request)

          {:ok, new_request, new_notifications, new_dependencies, resource_notification}
        else
          {:ok, new_request, new_notifications, new_dependencies}
        end

      {:continue, new_request, new_notifications} ->
        {:ok, new_request, new_notifications, []}

      {:error, error, new_request} ->
        {:error, error, new_request}

      {:wait, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications, new_dependencies}
    end
  end

  defp notify(state, notifications) do
    notifications =
      notifications
      |> Enum.uniq()
      |> Enum.reject(&MapSet.member?(state.notifications, &1))

    new_state_notifications = Enum.reduce(notifications, state.notifications, &MapSet.put(&2, &1))
    state = %{state | notifications: new_state_notifications}

    notifications
    |> Request.sort_and_clean_notifications()
    |> Enum.reduce(state, fn
      {:requests, requests}, state ->
        state
        |> add_requests(requests)

      {:set_extra_data, key, value}, state ->
        %{state | data: Map.put(state.data, key, value)}

      %Ash.Notifier.Notification{} = resource_notification, state ->
        add_resource_notification(state, resource_notification)

      {receiver_path, request_path, field, value}, state ->
        receiver_request = Enum.find(state.requests, &(&1.path == receiver_path))

        {:continue, new_request} =
          Request.receive_field(receiver_request, request_path, field, value)

        state
        |> Map.update!(:dependencies, fn dependencies ->
          Map.update!(dependencies, receiver_request.path, fn deps ->
            MapSet.delete(deps, {request_path, field})
          end)
        end)
        |> Map.update!(:reverse_dependencies, fn dependencies ->
          Map.update!(dependencies, request_path, fn deps ->
            MapSet.delete(deps, {receiver_request.path, field})
          end)
        end)
        |> replace_request(new_request)
    end)
  end

  defp store_dependencies(state, dependencies) do
    dependencies
    |> Enum.uniq()
    |> Enum.reject(fn {request_path, dep} = path_and_field ->
      MapSet.member?(state.dependencies_seen, path_and_field) ||
        dep in (state.dependencies[request_path] || [])
    end)
    |> Enum.reduce(
      state,
      fn {request_path, dep} = seen_dep, state ->
        dep_path = :lists.droplast(dep)
        dep_field = List.last(dep)

        depended_on_request = Enum.find(state.requests, &(&1.path == dep_path))

        if !depended_on_request do
          raise "Engine Error: No request found with path #{inspect(dep_path)}. Available paths:\n #{Enum.map_join(state.requests, "\n", &inspect(&1.path))}"
        end

        # we want to send things from non async requests
        # after we've sent all info to async requests
        unsent_dependencies =
          if depended_on_request.async? do
            state.unsent_dependencies ++ [{request_path, dep}]
          else
            [{request_path, dep} | state.unsent_dependencies]
          end

        %{
          state
          | dependencies:
              state.dependencies
              |> Map.put_new_lazy(request_path, fn -> MapSet.new() end)
              |> Map.update!(request_path, &MapSet.put(&1, {dep_path, dep_field})),
            reverse_dependencies:
              state.reverse_dependencies
              |> Map.put_new_lazy(dep_path, fn -> MapSet.new() end)
              |> Map.update!(dep_path, &MapSet.put(&1, {request_path, dep_field})),
            dependencies_seen: MapSet.put(state.dependencies_seen, seen_dep),
            unsent_dependencies: unsent_dependencies
        }
      end
    )
  end

  defp notify_local_request(
         state,
         depended_on_request,
         request,
         field
       ) do
    case Request.send_field(depended_on_request, request.path, field) do
      {:ok, new_request, new_notifications} ->
        state = replace_request(state, new_request)
        {state, new_notifications, []}

      {:waiting, new_request, new_notifications, new_dependencies} ->
        new_dependencies = build_dependencies(new_request, new_dependencies)

        new_state = replace_request(state, new_request)

        {new_state, new_notifications, new_dependencies}

      {:error, error, new_request} ->
        new_state =
          state
          |> replace_request(%{new_request | state: :error})
          |> add_error(new_request.path, error)

        {new_state, [], []}
    end
  end

  defp add_error(state, path, errors) when is_list(errors) do
    Enum.reduce(errors, state, &add_error(&2, path, &1))
  end

  defp add_error(state, _path, error) do
    error = Ash.Error.to_ash_error(error)

    if error in state.errors do
      state
    else
      %{state | errors: [error | state.errors]}
    end
  end

  defp replace_request(state, request) do
    %{
      state
      | requests:
          Enum.map(state.requests, fn existing_request ->
            if existing_request.path == request.path do
              request
            else
              existing_request
            end
          end)
    }
  end

  defp add_resource_notification(state, resource_notification) do
    if Ash.DataLayer.in_transaction?(resource_notification.resource) do
      %{state | resource_notifications: [resource_notification | state.resource_notifications]}
    else
      Ash.Notifier.notify(resource_notification)

      state
    end
  end

  def add_requests(state, requests) do
    requests =
      Enum.reject(requests, fn new_request ->
        Enum.find(state.requests, &(&1.path == new_request.path))
      end)

    {async, non_async} =
      requests
      |> Enum.map(fn request ->
        authorize? = request.authorize? and state.authorize?

        %{
          request
          | authorize?: authorize?,
            authorized?: !authorize?,
            actor: request.actor || state.actor,
            verbose?: request.verbose? || state.verbose?,
            async?: request.async? and state.async?
        }
      end)
      |> Enum.map(&Request.add_initial_authorizer_state/1)
      |> set_async()
      |> Enum.split_with(& &1.async?)

    %{state | requests: async ++ state.requests ++ non_async}
  end

  defp build_dependencies(request, dependencies) do
    Enum.map(dependencies, fn dep ->
      {request.path, dep}
    end)
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, id: id}, message, level) when is_function(message) do
    Logger.log(level, fn -> ["#{id}: ", message.()] end)
  end

  defp log(%{verbose?: true, id: id}, message, level) do
    Logger.log(level, fn -> ["#{id}: ", message] end)
  end

  defp log(_, _, _) do
    false
  end

  def put_nested_key(state, [key], value) do
    Map.put(state, key, value)
  end

  def put_nested_key(state, [key | rest], value) do
    case Map.fetch(state, key) do
      {:ok, nested_state} when is_map(nested_state) ->
        Map.put(state, key, put_nested_key(nested_state, rest, value))

      _ ->
        Map.put(state, key, put_nested_key(%{}, rest, value))
    end
  end

  def put_nested_key(state, key, value) do
    Map.put(state, key, value)
  end
end
