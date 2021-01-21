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
  the resources are configured and what capabilities the datalayer has. By implementing
  a generic "parallel engine", we can let the engine solve for the optimization. We simply
  have to express the various operations that must happen, and what other pieces of data
  they need in order to happen, and the engine handles the rest.

  Eventually, this module may (potentially) be used more explicitly, as a way to construct
  "sagas" or "multis" which represent a series of resource actions with linked up inputs.
  If each of those resource actions can be broken into its component requests, and the full
  set of requests can be processed, we can compose large series' of resource actions without
  having to figure out the most optimal way to do it. They will be done as fast as possible.
  But we have a long way to go before we get there.

  Check out the docs for `Ash.Engine.Request` for some more information. This is a private
  interface at the moment, though, so this documentation is just here to explain how it works
  it is not intended to give you enough information to use the engine directly.
  """
  defstruct [
    :api,
    :requests,
    :verbose?,
    :actor,
    :authorize?,
    :changeset,
    :runner_pid,
    :local_requests?,
    request_handlers: %{},
    active_requests: [],
    completed_requests: [],
    errored_requests: [],
    data: %{},
    errors: []
  ]

  alias Ash.Engine.{Request, Runner}

  use GenServer

  require Logger

  def run(request, api, opts \\ [])
  def run([], _api, _opts), do: {:error, :no_requests_provided}

  def run(requests, api, opts) do
    authorize? =
      if opts[:authorize?] == false do
        false
      else
        opts[:authorize?] || Keyword.has_key?(opts, :actor)
      end

    actor = opts[:actor]

    opts = Keyword.put(opts, :callers, [self() | Process.get(:"$callers", [])])

    # If the requests are invalid, this is a framework level error
    Request.validate_requests!(requests)

    requests =
      Enum.map(requests, fn request ->
        request = %{
          request
          | authorize?: request.authorize? and authorize?,
            actor: actor,
            verbose?: opts[:verbose?]
        }

        Request.add_initial_authorizer_state(request)
      end)

    transaction_result =
      maybe_transact(opts, requests, fn innermost_resource ->
        {local_requests, async_requests} = split_local_async_requests(requests)

        opts =
          opts
          |> Keyword.put(:requests, async_requests)
          |> Keyword.put(:local_requests?, !Enum.empty?(local_requests))
          |> Keyword.put(:runner_pid, self())
          |> Keyword.put(:api, api)

        run_requests(async_requests, local_requests, opts, innermost_resource)
      end)

    case transaction_result do
      {:ok, %{errors: [], resource_notifications: resource_notifications} = result} ->
        unsent = Ash.Notifier.notify(resource_notifications)

        {:ok, %{result | resource_notifications: unsent}}

      {:error, runner} ->
        {:error, runner}
    end
  end

  defp run_requests(async_requests, local_requests, opts, innermost_resource) do
    if async_requests == [] do
      run_and_return_or_rollback(local_requests, opts, innermost_resource)
    else
      Process.flag(:trap_exit, true)

      {:ok, pid} = GenServer.start(__MODULE__, opts)
      ref = Process.monitor(pid)

      try do
        receive do
          {:pid_info, pid_info} ->
            run_and_return_or_rollback(
              local_requests,
              opts,
              innermost_resource,
              pid,
              pid_info
            )
        end
      after
        Process.demonitor(ref, [:flush])
      end
    end
  end

  defp run_and_return_or_rollback(
         local_requests,
         opts,
         innermost_resource,
         pid \\ nil,
         pid_info \\ %{}
       ) do
    case Runner.run(local_requests, opts[:verbose?], pid, pid_info) do
      %{errors: errors} = runner when errors == [] ->
        {:ok, runner}

      runner ->
        rollback_or_return(innermost_resource, runner)
    end
  end

  defp rollback_or_return(innermost_resource, runner) do
    if innermost_resource do
      Ash.Resource.rollback(innermost_resource, runner)
    else
      {:error, runner}
    end
  end

  defp maybe_transact(opts, requests, func) do
    if opts[:transaction?] do
      requests
      |> Enum.map(& &1.resource)
      |> Enum.uniq()
      |> Enum.filter(&Ash.Resource.data_layer_can?(&1, :transact))
      |> do_in_transaction(func)
    else
      func.(nil)
    end
  end

  defp do_in_transaction(resources, func, innnermost \\ nil)

  defp do_in_transaction([], func, innermost_resource) do
    func.(innermost_resource)
  end

  defp do_in_transaction([resource | rest], func, _innermost) do
    Ash.Resource.transaction(resource, fn ->
      case do_in_transaction(rest, func, resource) do
        {:ok, value} ->
          value

        {:error, error} ->
          Ash.Resource.rollback(resource, error)
      end
    end)
  end

  def init(opts) do
    Process.put(:"$callers", opts[:callers])

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
    log(state, fn -> "Spawning request processes" end, :debug)

    new_state =
      Enum.reduce(state.requests, state, fn request, state ->
        {:ok, pid} =
          GenServer.start(Ash.Engine.RequestHandler,
            callers: [self() | Process.get("$callers", [])],
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
        {:register_dependency, receiver_path, request_handler_pid, dependency},
        state
      ) do
    path = :lists.droplast(dependency)
    field = List.last(dependency)

    case get_request(state, path) do
      {:error, _pid, request} ->
        case Map.get(request, field) do
          %Request.UnresolvedField{} ->
            send_or_cast(
              request_handler_pid,
              state.runner_pid,
              {:wont_receive, receiver_path, request.path, field}
            )

          value ->
            send_or_cast(
              request_handler_pid,
              state.runner_pid,
              {:field_value, receiver_path, request.path, field, value}
            )
        end

        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_cast({:local_requests_failed, _error}, state) do
    {:stop, {:shutdown, state}, state}
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

  def handle_cast({:error, error, request_handler_state}, state) do
    state
    |> log(fn -> "Error received from request_handler #{inspect(error)}" end)
    |> move_to_error(request_handler_state.request.path)
    |> add_error(request_handler_state.request.path, error)
    |> maybe_shutdown()
  end

  def handle_info({:EXIT, _pid, {:shutdown, {:error, error, request_handler_state}}}, state) do
    state
    |> log(fn -> "Error received from request_handler #{inspect(error)}" end)
    |> move_to_error(request_handler_state.request.path)
    |> add_error(request_handler_state.request.path, error)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, _pid, {:error, error, %Request{} = request}}, state) do
    state
    |> log(fn -> "Request exited in failure #{request.name}: #{inspect(error)}" end)
    |> move_to_error(request.path)
    |> add_error(request.path, error)
    |> maybe_shutdown()
  end

  def handle_info({:DOWN, _, _, pid, reason}, state) do
    {_state, _pid, request} = get_request(state, pid)

    state
    |> log(fn -> "Request exited in failure #{request.name}: #{inspect(reason)}" end)
    |> move_to_error(request.path)
    |> add_error(request.path, reason)
    |> maybe_shutdown()
  end

  defp send_or_cast(request_handler_pid, runner_pid, message) do
    if request_handler_pid == runner_pid do
      send(request_handler_pid, message)
    else
      GenServer.cast(request_handler_pid, message)
    end
  end

  defp get_request(state, pid) when is_pid(pid) do
    path = Map.get(state.request_handlers, pid)

    get_request(state, path, pid)
  end

  defp get_request(state, path, pid \\ nil) do
    case get_status(state, path) do
      nil ->
        nil

      status ->
        pid = pid || get_pid(state, path)
        {status, pid, Enum.find(state.requests, &(&1.path == path))}
    end
  end

  defp get_status(state, path) do
    cond do
      path in state.active_requests -> :active
      path in state.completed_requests -> :complete
      path in state.errored_requests -> :error
      true -> nil
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
    if Enum.any?(requests, fn request ->
         Ash.Resource.data_layer_can?(request.resource, :transact) &&
           Ash.Resource.in_transaction?(request.resource)
       end) do
      {requests, []}
    else
      {local, async} = Enum.split_with(requests, &must_be_local?/1)

      case {local, async} do
        {[], [first_async | rest]} ->
          {[first_async], rest}

        {local, async} ->
          {local, async}
      end
    end
  end

  defp must_be_local?(request) do
    not request.async? ||
      not Ash.Resource.data_layer_can?(request.resource, :async_engine)
  end

  defp maybe_shutdown(%{active_requests: [], local_requests?: false} = state) do
    log(state, fn -> "shutting down, completion criteria reached" end)
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
    log(state, fn -> "Initializing Engine with #{Enum.count(state.requests)} requests." end)
  end

  defp log(state, message, level \\ :info)

  defp log(%{verbose?: true} = state, message, level) do
    Logger.log(level, fn -> ["Engine: ", message.()] end)

    state
  end

  defp log(state, _, _) do
    state
  end

  defp add_error(state, path, errors) when is_list(errors) do
    Enum.reduce(errors, state, &add_error(&2, path, &1))
  end

  defp add_error(state, path, error) do
    path = List.wrap(path)

    error = Ash.Error.to_ash_error(error)

    %{state | errors: [Map.put(error, :path, path) | state.errors]}
  end
end
