defmodule Ash.Engine do
  defstruct [
    :api,
    :requests,
    :verbose?,
    :actor,
    :authorize?,
    request_handlers: %{},
    completed_requests: %{},
    errored_requests: [],
    data: %{},
    errors: []
  ]

  alias Ash.Engine.{Request}

  use GenServer

  require Logger

  def run(request, api, opts \\ [])
  def run([], _api, _opts), do: {:error, :no_requests_provided}

  def run(requests, api, opts) do
    opts =
      opts
      |> Keyword.put(:requests, requests)
      |> Keyword.put(:api, api)

    Process.flag(:trap_exit, true)
    {:ok, pid} = GenServer.start(__MODULE__, opts)
    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, _, _, {:shutdown, %{errored_requests: []} = state}} ->
        log(state, "Engine complete, graceful shutdown")

        state

      {:DOWN, ^ref, _, _, {:shutdown, state}} ->
        log(state, "Engine error, graceful shutdown")

        state
    end
  end

  def init(opts) do
    state =
      %__MODULE__{
        requests: opts[:requests] || [],
        verbose?: opts[:verbose?] || false,
        api: opts[:api],
        actor: opts[:actor],
        authorize?: opts[:authorize?] || false
      }
      |> log_engine_init()
      |> validate_unique_paths()
      |> validate_dependencies()

    {:ok, state, {:continue, :spawn_requests}}
  end

  def handle_continue(:spawn_requests, state) do
    log(state, "Spawning request processes", :debug)

    Enum.reduce(state.requests, state, fn request, state ->
      {:ok, pid} =
        GenServer.start(Ash.Engine.RequestHandler,
          request: request,
          verbose?: state.verbose?,
          actor?: state.actor,
          authorize?: state.authorize?,
          engine_pid: self()
        )

      Process.monitor(pid)

      %{state | request_handlers: Map.put(state.request_handlers, pid, request)}
    end)

    {:noreply, state}
  end

  def handle_cast({:register_dependency, request_handler_pid, dependency, optional?}, state) do
    path = :lists.droplast(dependency)
    field = List.last(dependency)

    case find_request(state, path) do
      {:active, pid, _request} ->
        GenServer.cast(pid, {:send_field, pid, field})

        {:noreply, state}

      {:complete, pid} ->
        GenServer.cast(pid, {:send_field, pid, field})

        {:noreply, state}

      {:error, request} ->
        case Map.get(request, field) do
          %Request.UnresolvedField{} ->
            unless optional? do
              GenServer.cast(request_handler_pid, {:wont_receive, request.path, field})
            end

          value ->
            GenServer.cast(request_handler_pid, {:field_value, request.path, field, value})
        end

        {:noreply, state}
    end
  end

  def handle_cast({:complete, pid, request_handler_state}, state) do
    state
    |> add_completed_request_handler(pid, request_handler_state)
    |> remove_request_handler(pid)
    |> add_data(request_handler_state)
    |> maybe_shutdown()
  end

  def handle_info({:EXIT, pid, {:shutdown, {:error, error, request_handler_state}}}, state) do
    state
    |> log("Error received from request_handler #{inspect(error)}")
    |> add_errored_request(request_handler_state.request)
    |> add_error(request_handler_state.request, error)
    |> remove_request_handler(pid)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, pid, {:error, error, %Request{} = request}}, state) do
    state
    |> log("Request exited in failure #{request.name}: #{inspect(error)}")
    |> add_errored_request(request)
    |> add_error(request, error)
    |> remove_request_handler(pid)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, pid, reason}, state) do
    request =
      Map.get(state.request_handlers, pid) || Map.get(state.completed_requests, pid).request

    state
    |> log("Request exited in failure #{request.name}: #{inspect(reason)}")
    |> add_errored_request(request)
    |> add_error(request, reason)
    |> remove_request_handler(pid)
    |> maybe_shutdown()
  end

  def put_nested_key(state, [key], value) do
    Map.put(state, key, value)
  end

  def put_nested_key(state, [key | rest], value) do
    case Map.fetch(state, key) do
      {:ok, nested_state} when is_map(nested_state) ->
        Map.put(state, key, put_nested_key(nested_state, rest, value))

      :error ->
        Map.put(state, key, put_nested_key(%{}, rest, value))
    end
  end

  def put_nested_key(state, key, value) do
    Map.put(state, key, value)
  end

  def fetch_nested_value(state, [key]) when is_map(state) do
    Map.fetch(state, key)
  end

  def fetch_nested_value(%Request.UnresolvedField{}, _), do: :error

  def fetch_nested_value(state, [key | rest]) when is_map(state) do
    case Map.fetch(state, key) do
      {:ok, value} -> fetch_nested_value(value, rest)
      :error -> :error
    end
  end

  def fetch_nested_value(state, key) when is_map(state) do
    Map.fetch(state, key)
  end

  defp add_data(state, request_handler_state) do
    if request_handler_state.request.write_to_data? do
      %{
        state
        | data:
            put_nested_key(
              state.data,
              request_handler_state.request.path,
              request_handler_state.request.data
            )
      }
    else
      state
    end
  end

  defp find_request(state, path) do
    with {:active, nil} <- {:active, do_find_request(state.request_handlers, path)},
         {:complete, nil} <- {:complete, do_find_request(state.completed_requests, path)},
         {:error, nil} <- {:error, do_find_request(state.errored_requests, path)} do
      raise "Unreachable!"
    else
      {:active, {pid, request}} -> {:active, pid, request}
      {:complete, request} -> {:complete, request}
      {:error, request} -> {:error, request}
    end
  end

  defp do_find_request(request_handlers, path) do
    if is_map(request_handlers) do
      Enum.find(request_handlers, fn {_pid, request} ->
        request.path == path
      end)
    else
      Enum.find(request_handlers, fn request ->
        request.path == path
      end)
    end
  end

  defp maybe_shutdown(%{request_handlers: request_handlers} = state)
       when request_handlers == %{} do
    {:stop, {:shutdown, state}, state}
  end

  defp maybe_shutdown(state) do
    {:noreply, state}
  end

  defp add_completed_request_handler(state, pid, request_handler_state) do
    %{
      state
      | completed_requests: Map.put(state.completed_requests, pid, request_handler_state)
    }
  end

  defp remove_request_handler(state, pid) do
    Map.update!(state, :request_handlers, &Map.delete(&1, pid))
  end

  defp add_errored_request(state, request) do
    Map.update!(state, :errored_requests, fn errored_requests ->
      [request | errored_requests]
    end)
  end

  defp log_engine_init(state) do
    log(state, "Initializing Engine with #{Enum.count(state.requests)} requests.")
    log(state, "Initial engine state: #{inspect(Map.delete(state, :request))}", :debug)
  end

  defp log(state, message, level \\ :info)

  defp log(%{verbose?: true} = state, message, level) do
    Logger.log(level, message)

    state
  end

  defp log(state, _, _) do
    state
  end

  defp validate_dependencies(state) do
    case Request.build_dependencies(state.requests) do
      # TODO: Have `build_dependencies/1` return an ash error
      {:error, {:impossible, path}} ->
        add_error(state, path, "Impossible path: #{inspect(path)} required by request.")

      {:ok, _requests} ->
        # TODO: no need to aggregate the full dependencies of
        state
    end
  end

  defp validate_unique_paths(state) do
    case Request.validate_unique_paths(state.requests) do
      :ok ->
        state

      {:error, paths} ->
        Enum.reduce(paths, state, &add_error(&2, &1, "Duplicate requests at path"))
    end
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
end
