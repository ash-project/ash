defmodule Ash.Engine do
  @moduledoc false

  defstruct [
    :ref,
    :id,
    :resolved_fields,
    :authorize?,
    :actor,
    :verbose?,
    :async?,
    :concurrency_limit,
    # :continue - Go until the end, gathering all errors
    # :stop - Stop at the first error
    failure_mode: :stop,
    return_notifications?: false,
    opts: [],
    requests: [],
    data: %{},
    dependencies_waiting_on_request: MapSet.new(),
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
  require Ash.Tracer
  require Logger

  def run(requests, opts \\ []) do
    resources =
      opts[:resource]
      |> List.wrap()
      |> Enum.concat(Enum.flat_map(requests, &(&1.touches_resources || [])))
      |> Enum.concat(Enum.flat_map(requests, &List.wrap(&1.resource)))
      |> Enum.concat(opts[:touches_resources] || [])
      |> Enum.reject(&is_nil/1)
      |> Enum.filter(&Ash.DataLayer.data_layer_can?(&1, :transact))

    cond do
      opts[:transaction?] && Enum.empty?(resources) ->
        raise "Engine invoked with `transaction?: true` but no resource, so no transaction could be started."

      opts[:transaction?] ->
        resources
        |> Enum.reject(&Ash.DataLayer.in_transaction?/1)
        |> case do
          [] ->
            do_run(requests, opts)

          resources ->
            notify? =
              if Process.get(:ash_started_transaction?) do
                false
              else
                Process.put(:ash_started_transaction?, true)
                true
              end

            try do
              Ash.DataLayer.transaction(
                resources,
                fn ->
                  case do_run(requests, opts) do
                    {:ok, result} ->
                      result

                    {:error, error} ->
                      Ash.DataLayer.rollback(Enum.at(resources, 0), error)
                  end
                end,
                opts[:timeout] || :infinity,
                transaction_metadata(opts)
              )
              |> case do
                {:ok, result} ->
                  if notify? do
                    saved_notifications = Process.delete(:ash_notifications) || []

                    remaining_notifications = result.resource_notifications ++ saved_notifications

                    {:ok, %{result | resource_notifications: remaining_notifications}}
                  else
                    {:ok, result}
                  end

                {:error, error} ->
                  {:error, error}
              end
            after
              if notify? do
                Process.delete(:ash_started_transaction?)
              end
            end
        end

      true ->
        task_with_timeout(
          fn ->
            do_run(requests, opts)
          end,
          opts[:resource],
          opts[:timeout],
          opts[:name],
          opts[:tracer]
        )
    end
    |> case do
      {:ok, %{resource_notifications: resource_notifications} = result} ->
        notifications =
          if Process.get(:ash_started_transaction?) && !opts[:return_notifications?] do
            current_notifications = Process.get(:ash_notifications, [])

            Process.put(
              :ash_notifications,
              current_notifications ++ resource_notifications
            )

            []
          else
            resource_notifications
          end

        {:ok, %{result | resource_notifications: notifications}}

      {:error, :timeout} ->
        {:error, Ash.Error.Invalid.Timeout.exception(timeout: opts[:timeout], name: opts[:name])}

      other ->
        other
    end
  end

  @doc false
  def task_with_timeout(fun, resource, timeout, name, tracer) do
    if !Application.get_env(:ash, :disable_async?) &&
         (is_nil(resource) ||
            Ash.DataLayer.data_layer_can?(resource, :async_engine)) && timeout &&
         timeout != :infinity && !Ash.DataLayer.in_transaction?(resource) do
      task =
        async(
          fun,
          tracer: tracer
        )

      try do
        case Task.await(task, timeout) do
          {:__exception__, e, stacktrace} ->
            reraise e, stacktrace

          other ->
            other
        end
      catch
        :exit, {:timeout, {Task, :await, [^task, timeout]}} ->
          {:error, Ash.Error.Invalid.Timeout.exception(timeout: timeout, name: name)}
      end
    else
      fun.()
    end
  end

  defp transaction_metadata(opts) do
    case opts[:transaction_reason] do
      %{metadata: metadata} = reason ->
        %{reason | metadata: Map.put(metadata, :actor, opts[:actor])}

      nil ->
        %{
          type: :custom,
          metadata: %{
            actor: opts[:actor]
          }
        }

      other ->
        other
    end
  end

  def do_run(requests, opts \\ []) do
    state =
      %__MODULE__{
        ref: make_ref(),
        id: Ash.UUID.generate(),
        resolved_fields: %{},
        actor: opts[:actor],
        return_notifications?: opts[:return_notifications?],
        failure_mode: opts[:failure_mode] || :stop,
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

    result = run_to_completion(state)

    status =
      case result.errors do
        [] -> :ok
        _ -> :error
      end

    {status,
     result.requests
     |> Enum.reduce(result, &add_data(&2, &1.path, &1.data))}
  end

  defp add_data(state, _path, %Ash.Engine.Request.UnresolvedField{}), do: state

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
    |> Enum.filter(fn item -> item && is_atom(item) end)
    |> Enum.any?(fn resource ->
      (Ash.DataLayer.data_layer_can?(resource, :transact) &&
         Ash.DataLayer.in_transaction?(resource)) ||
        not Ash.DataLayer.data_layer_can?(resource, :async_engine)
    end)
  end

  defp run_to_completion(state) do
    if complete?(state) do
      log(state, "Engine Complete")
      state
    else
      state
      |> run_iteration()
      |> case do
        ^state ->
          if state.tasks == [] && state.pending_tasks == [] do
            if state.errors == [] do
              detect_deadlocks(state)

              raise """
              Engine Deadlock! No async tasks and state is the same after iteration.
              #{long_breakdown(state)}
              """
            else
              state
            end
          else
            state
            |> start_pending_tasks()
            |> run_iteration()
            |> run_to_completion()
          end

        new_state ->
          run_to_completion(new_state)
      end
    end
  end

  defp complete?(%{failure_mode: :continue} = state) do
    Enum.all?(state.requests, &(&1.state in [:complete, :error]))
  end

  defp complete?(%{failure_mode: :stop} = state) do
    Enum.all?(state.requests, &(&1.state in [:complete])) ||
      Enum.any?(state.requests, &(&1.state in [:error]))
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

  defp stacktrace(%{stacktrace: %{stacktrace: stacktrace}})
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

  defp depends_on_summary(%{path: request_path} = request, state) do
    dependencies =
      state.dependencies_waiting_on_request
      |> Kernel.||([])
      |> Enum.filter(fn
        {^request_path, _} ->
          true

        _ ->
          false
      end)
      |> Enum.map(fn {_, dep} ->
        {:lists.droplast(dep), List.last(dep)}
      end)
      |> Enum.concat(state.dependencies[request_path] || [])

    if Enum.empty?(dependencies) do
      " state: #{request.state}"
    else
      " state: #{request.state} | depends on #{Enum.map_join(dependencies, ", ", &name_of(&1, state))}"
    end
  end

  defp name_of({path, dep}, state) do
    case Enum.find(state.requests, &(&1.path == path)) do
      nil ->
        "unknown dependency: #{inspect(path)} -> #{inspect(dep)}"

      request ->
        "#{request.name} -> #{inspect(dep)}"
    end
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
    state =
      state.tasks
      |> Enum.reduce(state, fn task, state ->
        case Task.yield(task, 0) do
          {:ok, {:__exception__, exception, stacktrace}} ->
            reraise exception, stacktrace

          {:ok, {request_path, result}} ->
            state = %{state | tasks: state.tasks -- [task]}
            request = Enum.find(state.requests, &(&1.path == request_path))

            new_request = %{request | async_fetch_state: {:fetched, result}}

            replace_request(state, new_request)

          nil ->
            state
        end
      end)

    case Enum.find(state.requests, &match?({:requested, _}, &1.async_fetch_state)) do
      nil ->
        {async, sync} =
          state.requests
          |> Enum.filter(fn request ->
            Enum.empty?(state.dependencies[request.path] || [])
          end)
          |> Enum.split_with(& &1.async?)

        new_state = Enum.reduce(async, state, &do_run_iteration(&2, &1))

        if state == new_state do
          Enum.reduce(sync, new_state, &do_run_iteration(&2, &1))
        else
          new_state
        end

      request ->
        {_, resolver_context} = request.async_fetch_state
        request = %{request | async_fetch_state: :fetching}

        pending_task = fn ->
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
              {request.path, request.data.resolver.(resolver_context)}
            end
          end
        end

        replace_request(
          %{state | pending_tasks: [pending_task | state.pending_tasks]},
          request
        )
    end
  end

  @doc false
  def async(func, opts) do
    ash_context = Ash.ProcessHelpers.get_context_for_transfer(opts)
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    Task.async(fn ->
      try do
        Ash.ProcessHelpers.transfer_context(ash_context, opts)

        func.()
      rescue
        e ->
          e =
            if Ash.Error.ash_error?(e) do
              if e.stacktrace && e.stacktrace.stacktrace do
                update_in(e.stacktrace.stacktrace, &(&1 ++ Enum.drop(stacktrace, 1)))
              else
                e
              end
            else
              e
            end

          {:__exception__, e, __STACKTRACE__ ++ Enum.drop(stacktrace, 1)}
      end
    end)
  end

  defp do_run_iteration(state, request) do
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

  def long_breakdown(state) do
    """
    #{errors(state)}
    #{completed_requests(state)}
    #{errored_requests(state)}
    #{pending_requests(state)}
    """
  end

  defp fully_advance_request(state, request) do
    case advance_request(request, state) do
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

    new_tasks = Enum.map(to_start, &async(&1, state.opts))

    %{state | tasks: state.tasks ++ new_tasks}
  end

  defp advance_request(%{state: state} = request, _state) when state in [:error] do
    {:ok, request, [], []}
  end

  defp advance_request(request, state) do
    case Request.next(request) do
      {:already_complete, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications, new_dependencies}

      {:complete, new_request, new_notifications, new_dependencies} ->
        if new_request.resource && new_request.notify? do
          resource_notification =
            sanitize_notification(Request.resource_notification(new_request), state)

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
        resource_notification = sanitize_notification(resource_notification, state)

        add_resource_notification(state, resource_notification)

      {receiver_path, request_path, field, value}, state ->
        receiver_request = Enum.find(state.requests, &(&1.path == receiver_path))

        {:continue, new_request} =
          Request.receive_field(receiver_request, request_path, field, value)

        state
        |> Map.update!(:dependencies, fn dependencies ->
          Map.update(dependencies, receiver_request.path, MapSet.new(), fn deps ->
            MapSet.delete(deps, {request_path, field})
          end)
        end)
        |> Map.update!(:reverse_dependencies, fn dependencies ->
          Map.update(dependencies, request_path, MapSet.new(), fn deps ->
            MapSet.delete(deps, {receiver_request.path, field})
          end)
        end)
        |> replace_request(new_request)
    end)
  end

  defp sanitize_notification(resource_notification, state) do
    %{
      resource_notification
      | from: self(),
        metadata:
          Map.merge(
            resource_notification.metadata || %{},
            state.opts[:notification_metadata] || %{}
          )
    }
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

        if depended_on_request do
          # we want to send things from non async requests
          # after we've sent all info to async requests
          unsent_dependencies =
            if depended_on_request && depended_on_request.async? do
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
        else
          %{
            state
            | dependencies_waiting_on_request:
                MapSet.put(state.dependencies_waiting_on_request, {request_path, dep})
          }
        end
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
    if Ash.DataLayer.in_transaction?(resource_notification.resource) ||
         state.return_notifications? do
      %{state | resource_notifications: [resource_notification | state.resource_notifications]}
    else
      unsent = Ash.Notifier.notify(resource_notification)

      %{state | resource_notifications: unsent ++ state.resource_notifications}
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
        if Enum.find(state.requests, &(&1.path == request.path)) do
          raise """
          Attempted to add request #{inspect(request.path)} but it has already been added!
          """
        end

        authorize? = request.authorize? and state.authorize?

        %{
          request
          | authorize?: authorize?,
            authorized?: !authorize?,
            actor: request.actor || state.actor,
            verbose?: request.verbose? || state.verbose?,
            async?: request.async? and state.async?,
            tracer: state.opts[:tracer]
        }
      end)
      |> Enum.map(&Request.add_initial_authorizer_state/1)
      |> set_async()
      |> Enum.split_with(& &1.async?)

    %{state | requests: async ++ state.requests ++ non_async}
    |> add_dependencies_waiting_on_request(requests)
  end

  defp add_dependencies_waiting_on_request(state, new_requests) do
    state.dependencies_waiting_on_request
    |> Enum.reduce(state, fn
      {request_path, dep}, state ->
        dep_path = :lists.droplast(dep)

        if Enum.any?(new_requests, &(&1.path == dep_path)) do
          %{
            state
            | unsent_dependencies: [{request_path, dep} | state.unsent_dependencies],
              dependencies_waiting_on_request:
                MapSet.delete(state.dependencies_waiting_on_request, {request_path, dep})
          }
        else
          state
        end
    end)
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
