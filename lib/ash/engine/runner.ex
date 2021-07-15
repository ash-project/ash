defmodule Ash.Engine.Runner do
  @moduledoc false
  defstruct [
    :engine_pid,
    :ref,
    notified_of_complete?: false,
    local_failed?: false,
    requests: [],
    completed: [],
    errors: [],
    changeset: %{},
    data: %{},
    engine_complete?: false,
    pid_info: %{},
    dependencies: %{},
    resource_notifications: [],
    verbose?: false
  ]

  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Error.Framework.SynchronousEngineStuck

  require Logger

  def run(requests, verbose?, ref, engine_pid \\ nil, pid_info \\ %{}) do
    changeset =
      Enum.find_value(requests, fn request ->
        if request.manage_changeset? && not match?(%Request.UnresolvedField{}, request.changeset) do
          request.changeset
        end
      end)

    state = %__MODULE__{
      requests: requests,
      verbose?: verbose?,
      engine_pid: engine_pid,
      pid_info: pid_info,
      ref: ref,
      data: %{verbose?: verbose?},
      changeset: changeset,
      resource_notifications: []
    }

    log(state, fn ->
      "Synchronous engine starting - #{Enum.map_join(requests, ", ", & &1.name)}"
    end)

    new_state = run_to_completion(state)

    new_state.requests
    |> Enum.reduce(new_state, &add_data(&2, &1.path, &1.data))
    |> Map.put(:resource_notifications, new_state.resource_notifications)
  end

  def run_to_completion(state) do
    # This allows for publishing any dependencies

    if Enum.all?(state.requests, &(&1.state in [:complete, :error])) do
      state = run_iteration(state)

      if Enum.all?(state.requests, &(&1.state in [:complete, :error])) do
        if state.engine_pid do
          wait_for_engine(state, true)
        else
          log(state, fn -> "Synchronous engine complete." end)
          state
        end
      else
        do_run_to_completion(state)
      end
    else
      do_run_to_completion(state)
    end
  end

  defp do_run_to_completion(state) do
    case run_iteration(state) do
      new_state when new_state == state ->
        if new_state.engine_pid do
          wait_for_engine(new_state, false)
        else
          if new_state.errors == [] do
            log(state, fn -> "Synchronous engine stuck:\n\n#{stuck_report(state)}" end)
            GenServer.cast(state.engine_pid, :log_stuck_report)
            add_error(new_state, :__engine__, SynchronousEngineStuck.exception([]))
          else
            new_state
          end
        end

      new_state ->
        run_to_completion(new_state)
    end
  end

  defp stuck_report(state) do
    Enum.map_join(state.requests, "\n", fn request ->
      if request.state in [:complete, :error] do
        to_string(request.name) <> ": " <> "#{inspect(request.state)}"
      else
        case Request.next(request) do
          {:wait, _, _, []} ->
            to_string(request.name) <> ": Waiting on nothing in state #{inspect(request.state)}"

          {:wait, _, _, dependencies} ->
            to_string(request.name) <> ": Waiting on #{dependency_names(dependencies, state)}"

          _other ->
            to_string(request.name) <> ": Not waiting, not complete"
        end
      end
    end)
  end

  defp dependency_names(dependencies, state) do
    Enum.map_join(dependencies, ", ", fn dependency ->
      path = :lists.droplast(dependency)
      field = List.last(dependency)

      request = Enum.find(state.requests, &(&1.path == path))

      request.name <> ": " <> to_string(field)
    end)
  end

  defp wait_for_engine(%{engine_pid: engine_pid, ref: ref} = state, complete?) do
    timeout =
      if state.engine_complete? do
        0
      else
        :infinity
      end

    receive do
      {^ref, {:update_changeset, changeset}} ->
        run_to_completion(%{state | changeset: changeset})

      {^ref, {:wont_receive, receiver_path, path, field}} ->
        request = Enum.find(state.requests, &(&1.path == receiver_path))

        new_state =
          case Request.wont_receive(request, path, field) do
            {:stop, :dependency_failed, new_request} ->
              state
              |> notify_error({:dependency_failed, path})
              |> replace_request(%{new_request | state: :error})
          end

        run_to_completion(new_state)

      {^ref, {:send_field, receiver_path, pid, dep}} ->
        log(state, fn -> "notifying #{inspect(receiver_path)} of #{inspect(dep)}" end)
        path = :lists.droplast(dep)
        field = List.last(dep)
        request = Enum.find(state.requests, &(&1.path == path))

        new_state =
          case Request.send_field(request, receiver_path, field) do
            {:waiting, new_request, notifications, dependencies} ->
              new_dependencies = build_dependencies(new_request, dependencies)

              {new_state, new_notifications} =
                state
                |> replace_request(new_request)
                |> store_dependencies(new_dependencies)

              notify(new_state, notifications ++ new_notifications)

            {:ok, new_request, notifications} ->
              state
              |> replace_request(new_request)
              |> notify(notifications)

            {:error, error, new_request} ->
              if pid == self() do
                send(self(), {ref, {:wont_receive, receiver_path, new_request.path, field}})
              else
                GenServer.cast(pid, {:wont_receive, receiver_path, new_request.path, field})
              end

              state
              |> add_error(new_request.path, error)
              |> replace_request(%{new_request | state: :error})
          end

        run_to_completion(new_state)

      {^ref, {:field_value, receiver_path, request_path, field, value}} ->
        receiver_path =
          case receiver_path do
            {_, path} -> path
            path -> path
          end

        request = Enum.find(state.requests, &(&1.path == receiver_path))

        case Request.receive_field(request, request_path, field, value) do
          {:continue, new_request} ->
            state
            |> replace_request(new_request)
            |> run_to_completion()
        end

      {^ref, {:notification, resource_notification}} ->
        state
        |> add_resource_notification(resource_notification)
        |> run_to_completion()

      {^ref, {:data, path, data}} ->
        state
        |> add_data(path, data)
        |> run_to_completion()

      {^ref, {:requests, requests}} ->
        {local, async} =
          if Enum.any?(requests, fn request ->
               Ash.DataLayer.data_layer_can?(request.resource, :transact) &&
                 Ash.DataLayer.in_transaction?(request.resource)
             end) do
            {requests, []}
          else
            Enum.split_with(requests, &Ash.Engine.must_be_local?/1)
          end

        state =
          if Enum.empty?(async) do
            state
          else
            GenServer.cast(state.engine_pid, {:spawn_requests, async})
            runner_ref = state.ref

            receive do
              {:pid_info, pid_info, ^runner_ref} ->
                %{state | pid_info: pid_info}
            end
          end

        state
        |> handle_requests(local)
        |> run_to_completion()

      {:DOWN, _, _, ^engine_pid,
       {:shutdown, %{errored_requests: [], runner_ref: ^ref} = engine_state}} ->
        log(state, fn -> "Engine complete" end)
        handle_completion(state, engine_state, complete?, false)

      {:DOWN, _, _, ^engine_pid, {:shutdown, %{runner_ref: ^ref} = engine_state}} ->
        log(state, fn -> "Engine complete" end)
        handle_completion(state, engine_state, complete?, true)
    after
      timeout ->
        state
    end
  end

  defp flush(state) do
    ref = state.ref
    engine_pid = state.engine_pid

    receive do
      {^ref, _} ->
        flush(state)

      {:DOWN, _, _, ^engine_pid, _} ->
        flush(state)
    after
      0 ->
        state
    end
  end

  defp handle_completion(state, engine_state, complete?, engine_error?) do
    state = %{state | engine_complete?: true}

    new_state =
      if complete? do
        if engine_error? do
          log(engine_state, fn -> "Engine shutdown error" end)
        else
          log(engine_state, fn -> "Engine complete, graceful shutdown" end)
        end

        state
      else
        run_to_completion(state)
      end

    add_engine_state(new_state, engine_state)
  end

  defp add_engine_state(state, engine_state) do
    new_state = %{state | errors: engine_state.errors ++ state.errors}

    flush(new_state)
  end

  defp build_dependencies(request, dependencies) do
    Enum.map(dependencies, fn dep ->
      {request.path, dep}
    end)
  end

  defp run_iteration(state) do
    {new_state, notifications, dependencies} =
      Enum.reduce(state.requests, {state, [], []}, fn request,
                                                      {state, notifications, dependencies} ->
        {new_state, new_notifications, new_dependencies} = fully_advance_request(state, request)

        {new_state, notifications ++ new_notifications, new_dependencies ++ dependencies}
      end)

    new_state
    |> store_dependencies(dependencies, notifications)
    |> notify_engine_complete()
  end

  defp notify_engine_complete(state) do
    newly_completed =
      state.requests
      |> Enum.filter(&(&1.state in [:complete, :error]))
      |> Enum.reject(&(&1.path in state.completed))
      |> Enum.map(& &1.path)

    Enum.each(newly_completed, fn completed_path ->
      GenServer.cast(state.engine_pid, {:local_request_complete, completed_path})
    end)

    %{state | completed: state.completed ++ newly_completed}
  end

  defp store_dependencies(state, dependencies, notifications \\ []) do
    log(state, fn -> "Storing generated dependencies" end)

    {state, notifications, more_dependencies} =
      dependencies
      |> Enum.uniq()
      |> Enum.reduce({state, notifications, []}, fn {request_path, dep},
                                                    {state, notifications, dependencies} ->
        request = Enum.find(state.requests, &(&1.path == request_path))
        path = :lists.droplast(dep)
        field = List.last(dep)

        case Enum.find(state.requests, &(&1.path == path)) do
          nil ->
            pid = Map.get(state.pid_info, path)

            try do
              # If the request handler is down, the engine will fail and
              # we won't get stuck in a loop
              GenServer.call(pid, {:send_field, request_path, self(), dep}, :infinity)
            catch
              :exit, _ ->
                :ok
            end

            {state, notifications, dependencies}

          depended_on_request ->
            if request.error? || depended_on_request.error? do
              {state, notifications, dependencies}
            else
              notify_local_request(
                state,
                notifications,
                dependencies,
                depended_on_request,
                request,
                field
              )
            end
        end
      end)

    case more_dependencies do
      [] ->
        notify(state, notifications)

      more_dependencies ->
        store_dependencies(state, more_dependencies, notifications)
    end
  end

  defp notify_local_request(
         state,
         notifications,
         dependencies,
         depended_on_request,
         request,
         field
       ) do
    case Request.send_field(depended_on_request, request.path, field) do
      {:ok, new_request, new_notifications} ->
        {replace_request(state, new_request), notifications ++ new_notifications, dependencies}

      {:waiting, new_request, new_notifications, new_dependencies} ->
        new_dependencies = build_dependencies(new_request, new_dependencies)

        new_state = replace_request(state, new_request)

        {new_state, notifications ++ new_notifications, new_dependencies ++ dependencies}

      {:error, error, new_request} ->
        new_state =
          state
          |> notify_error(error)
          |> replace_request(%{new_request | state: :error, error?: true})
          |> add_error(new_request.path, error)
          |> replace_request(%{new_request | state: :error, error?: true})

        {new_state, notifications, dependencies}
    end
  end

  defp notify_error(state, error) do
    GenServer.cast(state.engine_pid, {:local_requests_failed, error})

    %{state | local_failed?: true}
  end

  defp notify(state, notifications) do
    log(state, fn -> "sending/updating requests with notifications" end)

    notifications
    |> List.wrap()
    |> Enum.uniq()
    |> Enum.reduce(state, fn
      {:requests, requests}, state ->
        unless Enum.empty?(requests) do
          GenServer.cast(state.engine_pid, {:new_requests, requests})
        end

        state

      {:set_extra_data, key, value}, state ->
        %{state | data: Map.put(state.data, key, value)}

      {:update_changeset, value}, state ->
        %{state | changeset: value}

      %Ash.Notifier.Notification{} = resource_notification, state ->
        add_resource_notification(state, resource_notification)

      {receiver_path, request_path, field, value}, state ->
        case Enum.find(state.requests, &(&1.path == receiver_path)) do
          nil ->
            pid = Map.get(state.pid_info, receiver_path)
            GenServer.cast(pid, {:field_value, receiver_path, request_path, field, value})

            state

          receiver_request ->
            {:continue, new_request} =
              Request.receive_field(receiver_request, request_path, field, value)

            replace_request(state, new_request)
        end
    end)
  end

  defp handle_requests(state, []) do
    state
  end

  defp handle_requests(state, requests) do
    # TODO: At some point we might run these requests async, e.g send some to the engine
    %{state | requests: state.requests ++ requests, notified_of_complete?: false}
  end

  defp replace_request(state, request) do
    %{
      state
      | requests:
          Enum.map(state.requests, fn existing_request ->
            if existing_request.id == request.id do
              request
            else
              existing_request
            end
          end)
    }
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

      {:error, error, notifications, new_request} ->
        new_state =
          state
          |> notify_error(error)
          |> add_error(new_request.path, error)
          |> replace_request(%{new_request | state: :error})

        {new_state, notifications, []}

      {:error, error, new_request} ->
        new_state =
          state
          |> notify_error(error)
          |> add_error(new_request.path, error)
          |> replace_request(%{new_request | state: :error})

        {new_state, [], []}
    end
  end

  defp add_resource_notification(state, resource_notification) do
    if Ash.DataLayer.in_transaction?(resource_notification.resource) do
      %{state | resource_notifications: [resource_notification | state.resource_notifications]}
    else
      Ash.Notifier.notify(resource_notification)

      state
    end
  end

  defp advance_request(%{state: :error} = request) do
    {:ok, request, [], []}
  end

  defp advance_request(request) do
    case Request.next(request) do
      {:already_complete, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications, new_dependencies}

      {:complete, new_request, new_notifications, new_dependencies} ->
        if new_request.notify? do
          resource_notification = Request.resource_notification(new_request)

          {:ok, new_request, new_notifications, new_dependencies, resource_notification}
        else
          {:ok, new_request, new_notifications, new_dependencies}
        end

      {:continue, new_request, new_notifications} ->
        {:ok, new_request, new_notifications, []}

      {:error, error, notifications, new_request} ->
        {:error, error, notifications, new_request}

      {:error, error, new_request} ->
        {:error, error, new_request}

      {:wait, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications, new_dependencies}
    end
  end

  defp add_data(state, path, data) do
    %{
      state
      | data: Engine.put_nested_key(state.data, path, data)
    }
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

  defp log(request, message, level \\ :debug)

  defp log(%{verbose?: true}, message, level) do
    Logger.log(level, fn -> ["Runner: ", message.()] end)
  end

  defp log(_, _, _) do
    false
  end
end
