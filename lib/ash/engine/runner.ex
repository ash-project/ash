defmodule Ash.Engine.Runner do
  defstruct [
    :engine_pid,
    notified_of_complete?: false,
    requests: [],
    errors: [],
    data: %{},
    pid_info: %{},
    dependencies: %{},
    verbose?: false
  ]

  alias Ash.Engine.{Request, RequestHandler}
  alias Ash.Engine

  require Logger

  def run(requests, verbose?, engine_pid \\ nil, pid_info \\ %{}) do
    state = %__MODULE__{
      requests: requests,
      verbose?: verbose?,
      engine_pid: engine_pid,
      pid_info: pid_info
    }

    log(state, "Synchronous engine starting - #{Enum.map_join(requests, ", ", & &1.name)}")

    new_state = run_to_completion(state)

    Enum.reduce(new_state.requests, new_state, &add_data(&2, &1.path, &1.data))
  end

  def run_to_completion(state) do
    if Enum.all?(state.requests, &(&1.state in [:complete, :error])) do
      # This allows for publishing any dependencies
      new_state = run_iteration(state)

      if new_state.engine_pid do
        new_state =
          if new_state.notified_of_complete? do
            new_state
          else
            log(new_state, "notifying engine of local request completion")
            GenServer.cast(new_state.engine_pid, :local_requests_complete)
            %{new_state | notified_of_complete?: true}
          end

        wait_for_engine(new_state, true)
      else
        log(state, "Synchronous engine complete.")
        new_state
      end
    else
      case run_iteration(state) do
        new_state when new_state == state ->
          if new_state.engine_pid do
            wait_for_engine(new_state, false)
          else
            log(state, "Synchronous engine stuck:\n\n#{stuck_report(state)}")
            add_error(new_state, :__engine__, "Synchronous engine stuck")
          end

        new_state ->
          run_to_completion(new_state)
      end
    end
  end

  defp stuck_report(state) do
    Enum.map_join(state.requests, "\n", fn request ->
      if request.state in [:complete, :error] do
        request.name <> ": " <> "#{request.state}"
      else
        case Request.next(request) do
          {:wait, _, _, []} ->
            request.name <> ": Waiting on nothing in state #{inspect(request.state)}"

          {:wait, _, _, dependencies} ->
            request.name <> ": Waiting on #{dependency_names(dependencies, state)}"

          _other ->
            request.name <> ": Not waiting, not complete"
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

  defp wait_for_engine(state, complete?) do
    engine_pid = state.engine_pid
    log(state, "waiting for engine")

    receive do
      {:"$gen_cast", message} ->
        log(state, "Received #{inspect(message)}")

        message
        |> fake_handle_cast(state)
        |> run_to_completion()

      {:data, path, data} ->
        state
        |> add_data(path, data)
        |> wait_for_engine(complete?)

      {:DOWN, _, _, ^engine_pid, {:shutdown, %{errored_requests: []} = engine_state}} ->
        log(state, "Engine complete")
        handle_completion(state, engine_state, complete?, false)

      {:DOWN, _, _, ^engine_pid, {:shutdown, engine_state}} ->
        log(state, "Engine complete")
        handle_completion(state, engine_state, complete?, true)
    end
  end

  defp handle_completion(state, engine_state, complete?, engine_error?) do
    new_state =
      if complete? do
        if engine_error? do
          log(engine_state, "Engine shutdown error")
        else
          log(engine_state, "Engine complete, graceful shutdown")
        end

        state
      else
        new_state = run_to_completion(state)

        Enum.reduce(new_state.requests, new_state, fn request, state ->
          if request.state not in [:complete, :error] do
            state
            |> add_error(request.path, "Dependencies not met before shutdown")
            |> replace_request(%{request | state: :error})
          else
            state
          end
        end)
      end

    add_engine_state(new_state, engine_state)
  end

  defp add_engine_state(state, engine_state) do
    new_state = %{state | errors: engine_state.errors ++ state.errors}

    log(state, "waiting for engine data")
    receive_data(new_state)
  end

  defp receive_data(state) do
    receive do
      {:data, path, data} ->
        state
        |> add_data(path, data)
        |> receive_data()
    after
      0 ->
        state
    end
  end

  defp fake_handle_cast({:wont_receive, receiver_path, path, field}, state) do
    request = Map.get(state.requests, receiver_path)

    case Request.wont_receive(request, path, field) do
      {:stop, :dependency_failed, new_request} ->
        replace_request(state, %{new_request | state: :error})
    end
  end

  defp fake_handle_cast({:send_field, receiver_path, pid, dep, optional?}, state) do
    log(state, "notifying #{inspect(receiver_path)} of #{inspect(dep)}")
    path = :lists.droplast(dep)
    field = List.last(dep)
    request = Enum.find(state.requests, &(&1.path == path))

    case Request.send_field(request, receiver_path, field, optional?) do
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
        unless optional? do
          Engine.send_wont_receive(pid, receiver_path, new_request.path, field)
        end

        state
        |> add_error(new_request.path, error)
        |> replace_request(%{new_request | state: :error})
    end
  end

  defp fake_handle_cast({:field_value, receiver_path, request_path, field, value}, state) do
    request = Enum.find(state.requests, &(&1.path == receiver_path))

    case Request.receive_field(request, request_path, field, value) do
      {:continue, new_request} ->
        replace_request(state, new_request)
    end
  end

  defp build_dependencies(request, dependencies) do
    Enum.map(dependencies, fn {dep, optional?} ->
      {request.path, dep, optional?}
    end)
  end

  defp run_iteration(state) do
    {new_state, notifications, dependencies} =
      Enum.reduce(state.requests, {state, [], []}, fn request,
                                                      {state, notifications, dependencies} ->
        {new_state, new_notifications, new_dependencies} = fully_advance_request(state, request)

        {new_state, notifications ++ new_notifications, new_dependencies ++ dependencies}
      end)

    store_dependencies(new_state, dependencies, notifications)
  end

  defp store_dependencies(state, dependencies, notifications \\ []) do
    log(state, "Storing generated dependencies")

    {state, notifications, more_dependencies} =
      dependencies
      |> Enum.uniq()
      |> Enum.reduce({state, notifications, []}, fn {request_path, dep, optional?},
                                                    {state, notifications, dependencies} ->
        request = Enum.find(state.requests, &(&1.path == request_path))
        path = :lists.droplast(dep)
        field = List.last(dep)

        case Enum.find(state.requests, &(&1.path == path)) do
          nil ->
            pid = Map.get(state.pid_info, path)

            GenServer.cast(pid, {:send_field, path, self(), dep, optional?})

            {state, notifications, dependencies}

          depended_on_request ->
            case Request.send_field(depended_on_request, request.path, field, optional?) do
              {:ok, new_request, new_notifications} ->
                {replace_request(state, new_request), notifications ++ new_notifications,
                 dependencies}

              {:waiting, new_request, new_notifications, new_dependencies} ->
                new_dependencies = build_dependencies(new_request, new_dependencies)

                new_state = replace_request(state, new_request)

                {new_state, notifications ++ new_notifications, new_dependencies ++ dependencies}

              {:error, error, new_request} ->
                new_state =
                  state
                  |> replace_request(%{new_request | state: :error})
                  |> add_error(new_request.path, error)

                new_state =
                  if optional? do
                    new_state
                  else
                    new_state
                    |> add_error(request.path, "dependency failed")
                    |> replace_request(%{new_request | state: :error, error: error})
                  end

                {new_state, notifications, dependencies}
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

  defp notify(state, notifications) do
    log(state, "sending/updating requests with notifications")

    notifications
    |> List.wrap()
    |> Enum.uniq()
    |> Enum.reduce(state, fn {receiver_path, request_path, field, value}, state ->
      case Enum.find(state.requests, &(&1.path == receiver_path)) do
        nil ->
          pid = Map.get(state.pid_info, receiver_path)

          RequestHandler.send_field_value(
            pid,
            receiver_path,
            request_path,
            field,
            value
          )

          state

        receiver_request ->
          case Request.receive_field(receiver_request, request_path, field, value) do
            {:continue, new_request} ->
              replace_request(state, new_request)
          end
      end
    end)
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
    IO.inspect("advancing request")

    case advance_request(request) do
      {:ok, new_request, notifications, dependencies} ->
        new_state = replace_request(state, new_request)

        new_dependencies = build_dependencies(new_request, dependencies)

        {new_state, notifications, new_dependencies}

      {:error, error, new_request} ->
        new_state =
          state
          |> add_error(new_request.path, error)
          |> replace_request(%{new_request | state: :error})

        {new_state, [], []}
    end
  end

  defp advance_request(%{state: :error} = request) do
    {:ok, request, [], []}
  end

  defp advance_request(request) do
    IO.inspect("GOING IN")

    try do
      raise "What"
    rescue
      _ ->
        IO.inspect(__STACKTRACE__)

        :ok
    end

    case Request.next(request) do
      {:already_complete, new_request, new_notifications, new_dependencies} ->
        # when complete in [:complete, :already_complete] ->

        try do
          raise "What"
        rescue
          _ ->
            IO.inspect(__STACKTRACE__)

            :ok
        end

        IO.inspect(new_request.data, label: "after 1.5")
        {:ok, new_request, new_notifications, new_dependencies}

      {:complete, new_request, new_notifications, new_dependencies} ->
        # when complete in [:complete, :already_complete] ->

        try do
          raise "What"
        rescue
          _ ->
            IO.inspect(__STACKTRACE__)

            :ok
        end

        IO.inspect(new_request.data, label: "after 1")
        {:ok, new_request, new_notifications, new_dependencies}

      {:continue, new_request, new_notifications} ->
        IO.inspect(new_request.data, label: "after2")
        {:ok, new_request, new_notifications, []}

      {:error, error, new_request} ->
        IO.inspect(new_request.data, label: "after3")
        {:error, error, new_request}

      {:wait, new_request, new_notifications, new_dependencies} ->
        IO.inspect(new_request.data, label: "after4")
        {:ok, new_request, new_notifications, new_dependencies}
    end
  end

  defp add_data(state, path, data) do
    %{
      state
      | data: Engine.put_nested_key(state.data, path, data)
    }
  end

  defp add_error(state, path, error) do
    path = List.wrap(path)
    error = to_ash_error(error)

    %{state | errors: [Map.put(error, :path, path) | state.errors]}
  end

  defp to_ash_error(error) do
    if Ash.ash_error?(error) do
      error
    else
      Ash.Error.Unknown.exception(error: error)
    end
  end

  defp log(request, message, level \\ :debug)

  defp log(%{verbose?: true}, message, level) do
    Logger.log(level, "Runner: " <> message)
  end

  defp log(_, _, _) do
    false
  end
end
