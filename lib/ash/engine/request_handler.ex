defmodule Ash.Engine.RequestHandler do
  defstruct [
    :request,
    :engine_pid,
    :authorizer_state,
    :verbose?,
    :authorize?,
    :actor,
    :dependency_data,
    :strict_check_complete?,
    :state,
    dependencies_requested: [],
    dependencies_to_send: %{}
  ]

  use GenServer
  require Logger

  # TODO: as an optimization, make the authorizer_state global
  # to all request_handlers (using an agent or something)

  alias Ash.Engine.{Authorizer, Request}

  ## If not bypass strict check, then the engine needs to ensure
  # that a scenario is reality *at strict check time*
  # next step after strict checking

  def init(opts) do
    state = %__MODULE__{
      engine_pid: opts[:engine_pid],
      request: %{opts[:request] | verbose?: opts[:verbose?] || false},
      verbose?: opts[:verbose?] || false,
      authorizer_state: %{},
      dependency_data: %{},
      actor: opts[:actor],
      authorize?: opts[:authorize?],
      strict_check_complete?: false,
      state: :init
    }

    state = add_initial_authorizer_state(state)

    log(state, "Starting request")

    {:ok, %{state | state: :strict_check}, {:continue, :next}}
  end

  def handle_continue(:next, %{request: %{authorize?: false}, state: :strict_check} = state) do
    log(state, "Skipping strict check due to `authorize?: false`")
    {:noreply, %{state | state: :fetch_data, strict_check_complete?: true}, {:continue, :next}}
  end

  def handle_continue(:next, %{state: :strict_check} = state) do
    case Ash.authorizers(state.request.resource) do
      [] ->
        log(state, "No authorizers found, skipping strict check")

        {:noreply, %{state | state: :fetch_data, strict_check_complete?: true},
         {:continue, :next}}

      authorizers ->
        case strict_check(authorizers, state) do
          {:ok, new_state, true} ->
            log(state, "Strict check complete")
            new_state = %{new_state | strict_check_complete?: true, state: :fetch_data}
            {:noreply, new_state, {:continue, :next}}

          {:ok, new_state, false} ->
            log(state, "Strict check incomplete, waiting on dependencies")
            {:noreply, new_state}

          {:error, error} ->
            log(state, "Strict checking failed")
            {:stop, {:error, error, state.request}, state}
        end
    end
  end

  def handle_continue(:next, %{state: :fetch_data} = state) do
    case try_resolve_local(state, [:data], false) do
      {:skipped, _, _} ->
        raise "unreachable!"

      {:ok, state, []} ->
        {:noreply, %{state | state: :check}, {:continue, :next}}

      {:ok, state, _} ->
        {:noreply, state}

      {:error, error} ->
        {:stop, {:error, error, state.request}, state}
    end
  end

  def handle_continue(:next, %{request: %{authorize?: false}, state: :check} = state) do
    log(state, "Skipping check due to `authorize?: false`")
    complete(state)
    {:noreply, %{state | state: :complete}, {:continue, :next}}
  end

  def handle_continue(:next, %{state: :check} = state) do
    case Ash.authorizers(state.request.resource) do
      [] ->
        log(state, "No authorizers found, skipping check")
        complete(state)
        {:noreply, %{state | state: :complete}, {:continue, :next}}

      authorizers ->
        case check(authorizers, state) do
          {:ok, new_state, true} ->
            log(new_state, "Check complete")
            complete(new_state)

            {:noreply, %{state | state: :complete}, {:continue, :next}}

          {:ok, new_state, false} ->
            log(state, "Check incomplete, waiting on dependencies")
            {:noreply, new_state}

          {:error, error} ->
            log(state, "Check failed")
            {:stop, {:error, error, state.request}, state}
        end
    end
  end

  def handle_continue(:next, %{state: :complete} = state) do
    {:noreply, state}
  end

  def handle_cast({:wont_receive, path, field}, state) do
    log(state, "Quitting due to never receiving dependency #{inspect(path ++ [field])}")

    {:stop, {:error, "Dependency failed to resolve: #{inspect(path ++ [field])}"}, state}
  end

  def handle_cast({:send_field, pid, field}, state) do
    log(state, "Attempting to send #{field} to #{inspect(pid)}")

    case try_resolve_local(state, [field], false) do
      {:skipped, _, _} ->
        log(state, "Field could not be resolved #{field}, registering dependency")
        {:noreply, store_dependency(state, field, pid)}

      {:ok, new_state, _} ->
        case Map.get(new_state.request, field) do
          %Request.UnresolvedField{} ->
            log(state, "Field could not be resolved #{field}, registering dependency")
            {:noreply, store_dependency(state, field, pid)}

          value ->
            log(state, "Field value for #{field} sent")
            GenServer.cast(pid, {:field_value, state.request.path, field, value})

            {:noreply, new_state}
        end

      {:error, error} ->
        log(state, "Error resolving #{field}")
        GenServer.cast(pid, {:wont_receive, state.request.path, field})

        {:stop, {:error, error, state.request}, state}
    end
  end

  def handle_cast({:field_value, path, field, value}, state) do
    put_dependency_data(state, path ++ [field], value)

    {:noreply, state, {:continue, :next}}
  end

  defp store_dependency(state, field, pid) do
    new_deps_to_send =
      Map.update(state.dependencies_to_send, field, [pid], fn pids -> [pid | pids] end)

    %{state | dependencies_to_send: new_deps_to_send}
  end

  defp complete(state) do
    log(state, "Request complete")
    GenServer.cast(state.engine_pid, {:complete, self(), state})
  end

  defp check(authorizers, state) do
    Enum.reduce_while(authorizers, {:ok, state, true}, fn authorizer, {:ok, state, all_passed?} ->
      case do_check(authorizer, state) do
        {:ok, new_state} ->
          log(state, "check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_state, all_passed?}}

        {:waiting, new_state, waiting_for} ->
          log(
            state,
            "waiting on dependencies: #{inspect(waiting_for)} for #{inspect(authorizer)}"
          )

          {:cont, {:ok, new_state, false}}

        {:error, error} ->
          log(state, "check failed for #{inspect(authorizer)}: #{inspect(error)}")

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_check(authorizer, state) do
    case authorizer_state(state, authorizer) do
      :authorized ->
        {:ok, state}

      _authorizer_state ->
        case missing_check_dependencies(authorizer, state) do
          [] ->
            case check_authorizer(authorizer, state) do
              :authorized ->
                {:ok, set_authorizer_state(state, authorizer, :authorized)}

              {:error, error} ->
                {:error, error}
            end

          deps ->
            deps =
              Enum.map(deps, fn dep ->
                state.request.path ++ [dep]
              end)

            case try_resolve(state, deps) do
              {:ok, new_state, []} ->
                do_check(authorizer, new_state)

              {:ok, new_state, waiting_for} ->
                {:waiting, new_state, waiting_for}

              {:error, error} ->
                {:error, error}
            end
        end
    end
  end

  defp strict_check(authorizers, state) do
    Enum.reduce_while(authorizers, {:ok, state, true}, fn authorizer, {:ok, state, all_passed?} ->
      case do_strict_check(authorizer, state) do
        {:ok, new_state} ->
          log(state, "strict check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_state, all_passed?}}

        {:waiting, new_state, waiting_for} ->
          log(
            state,
            "waiting on dependencies: #{inspect(waiting_for)} for #{inspect(authorizer)}"
          )

          {:cont, {:ok, new_state, false}}

        {:error, error} ->
          log(state, "strict check failed for #{inspect(authorizer)}: #{inspect(error)}")

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_strict_check(authorizer, state) do
    case missing_strict_check_dependencies?(authorizer, state) do
      [] ->
        case strict_check_authorizer(authorizer, state) do
          :authorized ->
            {:ok, set_authorizer_state(state, authorizer, :authorized)}

          {:continue, authorizer_state} ->
            {:ok, set_authorizer_state(state, authorizer, authorizer_state)}

          {:error, error} ->
            {:error, error}
        end

      deps ->
        deps =
          Enum.map(deps, fn dep ->
            state.request.path ++ [dep]
          end)

        case try_resolve(state, deps) do
          {:ok, new_state, []} ->
            do_strict_check(authorizer, new_state)

          {:ok, new_state, waiting_for} ->
            {:waiting, new_state, waiting_for}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp try_resolve(state, dep_or_deps, optional? \\ false) do
    dep_or_deps
    |> List.wrap()
    |> Enum.reduce_while({:ok, state, []}, fn dep, {:ok, state, skipped} ->
      if local_dep?(state, dep) do
        case try_resolve_local(state, dep, optional?) do
          {:skipped, state, other_deps} -> {:cont, {:ok, state, [dep | skipped] ++ other_deps}}
          {:ok, state, other_deps} -> {:cont, {:ok, state, skipped ++ other_deps}}
          {:error, error} -> {:halt, {:error, error}}
        end
      else
        case get_dependency_data(state, dep) do
          {:ok, _value} ->
            {:cont, {:ok, state, skipped}}

          :error ->
            new_state = register_dependency(state, dep, optional?)

            {:cont, {:ok, new_state, [dep | skipped]}}
        end
      end
    end)
  end

  defp register_dependency(state, dep, optional?) do
    GenServer.cast(state.engine_pid, {:register_dependency, self(), dep, optional?})

    %{state | dependencies_requested: [dep | state.dependencies_requested]}
  end

  defp try_resolve_local(state, dep, optional?) do
    field = List.last(dep)

    # Don't fetch request data if strict_check is not complete
    if field == :data && not state.strict_check_complete? do
      case state.request.data do
        %Request.UnresolvedField{deps: deps, optional_deps: optional_deps} ->
          with {:ok, new_state, _remaining_optional} <- try_resolve(state, optional_deps, true),
               {:ok, new_state, remaining_deps} <- try_resolve(new_state, deps, optional?) do
            {:skipped, new_state, remaining_deps}
          end

        _ ->
          {:skipped, state, []}
      end
    else
      case state.request do
        %{^field => %Request.UnresolvedField{} = unresolved} ->
          %{deps: deps, optional_deps: optional_deps, resolver: resolver} = unresolved

          with {:ok, new_state, _remaining_optional} <- try_resolve(state, optional_deps, true),
               {:ok, new_state, remaining_deps} <- try_resolve(new_state, deps, optional?) do
            resolver_context = resolver_context(new_state, deps ++ optional_deps)

            case resolver.(resolver_context) do
              {:ok, value} ->
                new_state = notify_dependents(new_state, field, value)
                new_state = %{new_state | request: Map.put(new_state.request, field, value)}

                new_state =
                  put_dependency_data(new_state, new_state.request.path ++ [field], value)

                {:ok, new_state, remaining_deps}

              {:error, error} ->
                {:error, error}
            end
          end

        %{^field => value} ->
          {:ok, put_dependency_data(state, dep, value), []}
      end
    end
  end

  defp notify_dependents(state, field, value) do
    case Map.fetch(state.dependencies_to_send, field) do
      {:ok, pids} ->
        Enum.each(pids, &GenServer.cast(&1, {:field_value, state.request.path, field, value}))
        %{state | dependencies_to_send: Map.delete(state.dependencies_to_send, field)}

      :error ->
        state
    end
  end

  defp get_dependency_data(state, dep) do
    Map.fetch(state.dependency_data, dep)
  end

  defp put_dependency_data(state, dep, value) do
    %{state | dependency_data: Map.put(state.dependency_data, dep, value)}
  end

  defp resolver_context(state, deps) do
    Enum.reduce(deps, %{}, fn dep, resolver_context ->
      case get_dependency_data(state, dep) do
        {:ok, value} ->
          Ash.Engine.put_nested_key(resolver_context, dep, value)

        :error ->
          resolver_context
      end
    end)
  end

  defp local_dep?(state, dep) do
    :lists.droplast(dep) == state.request.path
  end

  defp add_initial_authorizer_state(state) do
    state.request.resource
    |> Ash.authorizers()
    |> Enum.reduce(state, fn authorizer, state ->
      initial_state =
        Ash.Engine.Authorizer.initial_state(
          authorizer,
          state.actor,
          state.request.resource,
          state.request.action,
          state.verbose?
        )

      set_authorizer_state(state, authorizer, initial_state)
    end)
  end

  defp set_authorizer_state(state, authorizer, authorizer_state) do
    %{
      state
      | authorizer_state: Map.put(state.authorizer_state, authorizer, authorizer_state)
    }
  end

  defp authorizer_state(state, authorizer) do
    Map.get(state.authorizer_state, authorizer) || %{}
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, request: request}, message, level) do
    Logger.log(level, "#{request.name}: #{message}")
  end

  defp log(_, _, _) do
    false
  end
end
