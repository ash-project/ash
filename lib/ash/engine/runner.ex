defmodule Ash.Engine.Runner do
  defstruct [
    :engine_pid,
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

    log(state, "Synchronous engine starting")

    run_to_completion(state)
  end

  def run_to_completion(state) do
    if Enum.all?(state.requests, &(&1.state in [:complete, :error])) do
      # This allows for publishing any dependencies
      new_state = run_iteration(state)
      new_state = Enum.reduce(new_state.requests, state, &add_data(&2, &1.path, &1.data))

      if new_state.engine_pid do
        GenServer.cast(new_state.engine_pid, :local_requests_complete)
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
            log(state, "Synchronous engine stuck.")
            add_error(state, :__engine__, "Synchronous engine stuck")
          end

        new_state ->
          run_to_completion(new_state)
      end
    end
  end

  defp wait_for_engine(state, complete?) do
    engine_pid = state.engine_pid
    log(state, "waiting for engine")

    receive do
      {:"$gen_cast", message} ->
        fake_handle_cast(message, state)

      {:DOWN, _, _, ^engine_pid, {:shutdown, %{errored_requests: []} = engine_state}} ->
        handle_completion(state, engine_state, complete?, false)

      {:DOWN, _, _, ^engine_pid, {:shutdown, engine_state}} ->
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
    path = :lists.droplast(dep)
    field = List.last(dep)
    request = Enum.find(state.requests, &(&1.path == path))

    case Request.send_field(request, receiver_path, field, optional?) do
      {:waiting, new_request, notifications, dependencies} ->
        state
        |> replace_request(new_request)
        |> store_dependencies(new_request, dependencies)
        |> notify(notifications)

      {:ok, new_request, notifications} ->
        state
        |> replace_request(new_request)
        |> notify(notifications)

      {:error, error, new_request} ->
        Engine.send_wont_receive(pid, receiver_path, new_request.path, field, optional?)

        state
        |> add_error(new_request.path, error)
        |> replace_request(%{new_request | state: :error})
    end
  end

  defp fake_handle_cast({:field_value, receiver_path, field, value}, state) do
    request = Enum.find(state.requests, &(&1.path == receiver_path))

    case Request.receive_field(request, receiver_path, field, value) do
      {:continue, new_request} ->
        state
        |> replace_request(new_request)
        |> run_to_completion()
    end
  end

  defp run_iteration(state) do
    Enum.reduce(state.requests, state, fn request, state ->
      fully_advance_request(state, request)
    end)
  end

  defp store_dependencies(state, request, dependencies) do
    Enum.reduce(dependencies, state, fn {dep, optional?}, state ->
      field = List.last(dep)
      path = :lists.droplast(dep)

      case Enum.find(state.requests, &(&1.path == path)) do
        nil ->
          pid = Map.get(state.pid_info, path)

          GenServer.cast(pid, {:send_field, request.path, self(), field, optional?})

          state

        depended_on_request ->
          case Request.send_field(depended_on_request, request.path, field, optional?) do
            {:ok, new_request, notifications} ->
              state
              |> replace_request(new_request)
              |> notify(notifications)

            {:waiting, new_request, notifications, waiting_for} ->
              state
              |> replace_request(new_request)
              |> store_dependencies(new_request, waiting_for)
              |> notify(notifications)

            {:error, error, new_request} ->
              new_state =
                state
                |> replace_request(%{new_request | state: :error})
                |> add_error(new_request.path, error)

              if optional? do
                new_state
              else
                new_state
                |> add_error(request.path, "dependency failed")
                |> replace_request(%{request | error: error})
              end
          end
      end
    end)
  end

  defp notify(state, notifications) do
    Enum.reduce(List.wrap(notifications), state, fn {receiver_path, request_path, field, value},
                                                    state ->
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

        receiver_request ->
          case Request.receive_field(receiver_request, request_path, field, value) do
            {:continue, new_request} ->
              state
              |> replace_request(new_request)
              |> fully_advance_request(new_request)
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
    case advance_request(state, request) do
      {:ok, new_request, notifications, dependencies} ->
        state
        |> replace_request(new_request)
        |> store_dependencies(new_request, dependencies)
        |> notify(notifications)

      {:error, error, new_request} ->
        state
        |> add_error(new_request.path, error)
        |> replace_request(%{new_request | state: :error})
    end
  end

  defp advance_request(state, request, notifications \\ [], dependencies \\ []) do
    case Request.next(request) do
      {complete, new_request, new_notifications}
      when complete in [:complete, :already_complete] ->
        {:ok, new_request, new_notifications ++ notifications, dependencies}

      {:continue, new_request, new_notifications} ->
        advance_request(state, new_request, notifications ++ new_notifications, dependencies)

      {:error, error, new_request} ->
        {:error, error, new_request}

      {:wait, new_request, new_notifications, new_dependencies} ->
        {:ok, new_request, new_notifications ++ notifications, new_dependencies ++ dependencies}
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
    Logger.log(level, "#{message}")
  end

  defp log(_, _, _) do
    false
  end
end
