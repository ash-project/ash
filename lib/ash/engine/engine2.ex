defmodule Ash.Engine.Engine2 do
  defstruct [
    :api,
    :user,
    :requests,
    :verbose?,
    :skip_authorization?,
    :bypass_strict_access?,
    request_handlers: %{},
    errored_requests: [],
    data: %{},
    errors: [],
    facts: %{true => true, false => false}
  ]

  alias Ash.Authorization.SatSolver
  alias Ash.Authorization.Checker
  alias Ash.Engine.Request

  use GenServer

  require Logger

  def run([], _api, _opts), do: {:error, :no_requests_provided}

  def run(requests, api, opts) do
    opts =
      opts
      |> Keyword.put(:requests, requests)
      |> Keyword.put(:api, api)

    Process.flag(:trap_exit, true)
    {:ok, pid} = GenServer.start_link(__MODULE__, opts)
    ref = Process.monitor(pid)

    receive do
      {:EXIT, ^pid, {:shutdown, state}} ->
        log(state, "Engine complete, graceful shutdown")

        {:ok, state}

      {:EXIT, ^pid, reason} ->
        if opts[:verbose?] do
          Logger.warn("Engine complete, error shutdown: #{inspect(reason)}")
        end

        {:error, reason}

      {:DOWN, ^ref, _thing1, _thing2, _reason} = message ->
        IO.inspect(message)

        raise "unreachable DOWN message"
    end
  end

  def init(opts) do
    state = %__MODULE__{
      requests: opts[:requests] || [],
      verbose?: opts[:verbose?] || false,
      api: opts[:api],
      skip_authorization?: opts[:skip_authorization?] || false,
      user: opts[:user],
      bypass_strict_access?: opts[:bypass_strict_access] || false
    }

    new_state =
      state
      |> log_engine_init()
      |> validate_unique_paths()
      |> bypass_strict_access(opts)
      |> validate_dependencies()
      |> validate_has_rules()

    {:ok, new_state, {:continue, :spawn_requests}}
  end

  def handle_continue(:spawn_requests, state) do
    log(state, "Spawning request processes", :debug)

    Process.flag(:trap_exit, true)

    state =
      Enum.reduce(state.requests, state, fn request, state ->
        {:ok, pid} =
          GenServer.start_link(Ash.Engine.RequestHandler,
            request: request,
            verbose?: state.verbose?,
            engine_pid: self()
          )

        %{state | request_handlers: Map.put(state.request_handlers, pid, request)}
      end)

    {:noreply, state}
  end

  # Change this to `update_request`
  def handle_cast({:update_request, new_request}, state) do
    new_request_handlers =
      Enum.reduce(state.request_handlers, state.request_handlers, fn {pid, request},
                                                                     request_handlers ->
        if request.id == new_request.id do
          Map.put(request_handlers, pid, new_request)
        else
          request_handlers
        end
      end)

    if new_request.write_to_data? do
      new_data = Request.put_request(state.data, new_request)
      {:noreply, %{state | data: new_data, request_handlers: new_request_handlers}}
    else
      {:noreply, %{state | new_request_handlers: new_request_handlers}}
    end
  end

  def handle_call({:strict_check, request}, _, state) do
    log(state, "Strict checking #{request.name}", :debug)
    new_facts = Checker.strict_check(state.user, request, state.facts)

    case SatSolver.solve([request], new_facts) do
      {:ok, _scenarios} ->
        {:reply, :ok, %{state | facts: new_facts}}

      {:error, :unsatisfiable} ->
        {:reply, {:error, :unsatisfiable}, state}
    end
  end

  def handle_info({:EXIT, pid, {:shutdown, request_handler_state}}, state) do
    state
    |> log("Successfully completed request #{request_handler_state.request.name}")
    |> remove_request_handler(pid)
    |> maybe_shutdown()
  end

  def handle_info({:EXIT, pid, reason}, state) do
    request = Map.get(state.request_handlers, pid)

    state
    |> log("Request exited in failure #{request.name}: #{inspect(reason)}")
    |> add_errored_request(request)
    |> remove_request_handler(pid)
    |> maybe_shutdown()
  end

  defp maybe_shutdown(%{request_handlers: request_handlers} = state)
       when request_handlers == %{} do
    {:stop, {:shutdown, state}, state}
  end

  defp maybe_shutdown(state) do
    {:noreply, state}
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

  defp bypass_strict_access(engine, opts) do
    if opts[:bypass_strict_access?] do
      %{engine | requests: Enum.map(engine.requests, &Map.put(&1, :strict_access?, false))}
    else
      engine
    end
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

  defp validate_has_rules(%{skip_authorization?: true} = state), do: state

  defp validate_has_rules(state) do
    case Enum.find(state.requests, &Enum.empty?(&1.rules)) do
      nil ->
        state

      request ->
        add_error(state, request.path, "No authorization steps configured")
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
