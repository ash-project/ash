defmodule Ash.Engine do
  defstruct [
    :api,
    :requests,
    :verbose?,
    :actor,
    :authorize?,
    :runner_pid,
    :local_requests?,
    request_handlers: %{},
    active_requests: [],
    completed_requests: [],
    errored_requests: [],
    data: %{},
    errors: []
  ]

  alias Ash.Engine.{Request, RequestHandler, Runner}

  use GenServer

  require Logger

  def run(request, api, opts \\ [])
  def run([], _api, _opts), do: {:error, :no_requests_provided}

  def run(requests, api, opts) do
    authorize? = opts[:authorize?] || Keyword.has_key?(opts, :actor)
    actor = opts[:actor]

    case Request.validate_requests(requests) do
      :ok ->
        requests =
          Enum.map(requests, fn request ->
            %{
              request
              | authorize?: authorize?,
                actor: actor,
                verbose?: opts[:verbose?]
            }
          end)

        {local_requests, async_requests} = split_local_async_requests(requests)

        opts =
          opts
          |> Keyword.put(:requests, async_requests)
          |> Keyword.put(:local_requests?, !Enum.empty?(local_requests))
          |> Keyword.put(:runner_pid, self())
          |> Keyword.put(:api, api)

        if async_requests == [] do
          Runner.run(local_requests, nil, opts[:verbose?])
        else
          Process.flag(:trap_exit, true)
          {:ok, pid} = GenServer.start(__MODULE__, opts)
          _ = Process.monitor(pid)

          receive do
            {:pid_info, pid_info} ->
              Runner.run(local_requests, opts[:verbose?], pid, pid_info)
          end
        end
    end
  end

  def init(opts) do
    state =
      %__MODULE__{
        requests: opts[:requests],
        active_requests: Enum.map(opts[:requests], & &1.path),
        runner_pid: opts[:runner_pid],
        local_requests?: opts[:local_requests?],
        verbose?: opts[:verbose?] || false,
        api: opts[:api],
        actor: opts[:actor],
        authorize?: opts[:authorize?] || false
      }
      |> log_engine_init()

    {:ok, state, {:continue, :spawn_requests}}
  end

  def handle_continue(:spawn_requests, state) do
    log(state, "Spawning request processes", :debug)

    new_state =
      Enum.reduce(state.requests, state, fn request, state ->
        {:ok, pid} =
          GenServer.start(Ash.Engine.RequestHandler,
            request: request,
            verbose?: state.verbose?,
            actor?: state.actor,
            authorize?: state.authorize?,
            engine_pid: self(),
            runner_pid: state.runner_pid
          )

        Process.monitor(pid)

        %{
          state
          | request_handlers: Map.put(state.request_handlers, pid, request.path)
        }
      end)

    pid_info =
      Enum.into(new_state.request_handlers, %{}, fn {pid, path} ->
        {path, pid}
      end)

    if new_state.local_requests? do
      send(new_state.runner_pid, {:pid_info, pid_info})
    end

    Enum.each(new_state.request_handlers, fn {pid, _} ->
      send(pid, {:pid_info, pid_info})
    end)

    {:noreply, new_state}
  end

  def handle_cast(
        {:register_dependency, receiver_path, request_handler_pid, dependency, optional?},
        state
      ) do
    path = :lists.droplast(dependency)
    field = List.last(dependency)

    case get_request(state, path) do
      {:active, _pid, _request} ->
        {:noreply, state}

      {:complete, _pid, _request} ->
        {:noreply, state}

      {:error, _pid, request} ->
        case Map.get(request, field) do
          %Request.UnresolvedField{} ->
            send_wont_receive(request_handler_pid, receiver_path, request.path, field, optional?)

          value ->
            RequestHandler.send_field_value(
              request_handler_pid,
              receiver_path,
              request.path,
              field,
              value
            )
        end

        {:noreply, state}
    end
  end

  def handle_cast({:complete, path}, state) do
    state
    |> move_to_complete(path)
    |> maybe_shutdown()
  end

  def handle_cast(:local_requests_complete, state) do
    %{state | local_requests?: false}
    |> maybe_shutdown()
  end

  def send_wont_receive(pid, caller_path, request_path, field, true) do
    GenServer.cast(pid, {:wont_receive, caller_path, request_path, field})
  end

  def send_wont_receive(_pid, _caller_path, _request_path, _field, false) do
    :ok
  end

  def handle_info({:EXIT, _pid, {:shutdown, {:error, error, request_handler_state}}}, state) do
    state
    |> log("Error received from request_handler #{inspect(error)}")
    |> move_to_error(request_handler_state.request.path)
    |> add_error(request_handler_state.request, error)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, _pid, {:error, error, %Request{} = request}}, state) do
    state
    |> log("Request exited in failure #{request.name}: #{inspect(error)}")
    |> move_to_error(request.path)
    |> add_error(request, error)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, pid, reason}, state) do
    {_state, _pid, request} = get_request(state, pid)

    state
    |> log("Request exited in failure #{request.name}: #{inspect(reason)}")
    |> move_to_error(request.path)
    |> add_error(request, reason)
    |> maybe_shutdown()
  end

  defp get_request(state, pid) when is_pid(pid) do
    path = Map.get(state.request_handlers, pid)

    get_request(state, path, pid)
  end

  defp get_request(state, path, pid \\ nil) do
    status = get_status(state, path)
    pid = pid || get_pid(state, path)
    {status, pid, Enum.find(state.requests, &(&1.path == path))}
  end

  defp get_status(state, path) do
    cond do
      path in state.active_requests -> :active
      path in state.completed_requests -> :complete
      path in state.errored_requests -> :error
    end
  end

  defp get_pid(state, path) do
    Enum.find_value(state.request_handlers, fn {pid, request_path} ->
      if request_path == path do
        pid
      end
    end) || state.runner_pid
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

  defp split_local_async_requests(requests) do
    # TODO: check for transactions, run anything in a transaction synchronously
    case requests do
      [request] -> {[request], []}
      [request | rest] -> {[request], rest}
    end
  end

  defp maybe_shutdown(%{active_requests: [], local_requests?: false} = state) do
    {:stop, {:shutdown, state}, state}
  end

  defp maybe_shutdown(state) do
    {:noreply, state}
  end

  defp move_to_complete(state, path) do
    %{
      state
      | completed_requests: [path | state.completed_requests],
        active_requests: state.active_requests -- [path]
    }
  end

  defp move_to_error(state, path) do
    %{
      state
      | errored_requests: [path | state.completed_requests],
        active_requests: state.active_requests -- [path]
    }
  end

  defp log_engine_init(state) do
    log(state, "Initializing Engine with #{Enum.count(state.requests)} requests.")
  end

  defp log(state, message, level \\ :info)

  defp log(%{verbose?: true} = state, message, level) do
    Logger.log(level, message)

    state
  end

  defp log(state, _, _) do
    state
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
