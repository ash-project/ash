defmodule Ash.Engine.RequestHandler do
  defstruct [:request, :engine_pid, :verbose?, :skip_authorization?]
  use GenServer
  require Logger

  alias Ash.Engine.Request

  ## If not bypass strict check, then the engine needs to ensure
  # that a scenario is reality *at strict check time*
  # next step after strict checking

  def init(opts) do
    state = %__MODULE__{
      engine_pid: opts[:engine_pid],
      request: opts[:request],
      verbose?: opts[:verbose?] || false,
      skip_authorization?: opts[:skip_authorization?] || false
    }

    log(state, "Starting request")

    {:ok, state, {:continue, :strict_check}}
  end

  def handle_continue(:strict_check, %{skip_authorization?: false} = state) do
    log(state, "Skipping strict check due to `skip_authorization?` flag")
    {:stop, {:shutdown, state}, state}
  end

  def handle_continue(:strict_check, state) do
    if can_strict_check?(state.request) do
      log(state, "strict checking")

      case GenServer.call(state.engine_pid, {:strict_check, state.request}) do
        :ok ->
          log(state, "strict_check succeeded")
          new_request = Map.put(state.request, :strict_check_complete?, true)
          GenServer.cast(state.engine_pid, {:update_request, new_request})
          new_state = Map.put(state, :request, new_request)
          {:stop, {:shutdown, new_state}, new_state}

        {:error, :unsatisfiable} ->
          log(state, "strict_check failed")
          {:stop, {:error, :unsatisfiable}, state}
      end
    else
      {:noreply, state, {:continue, :resolve_non_data_dependencies}}
    end
  end

  def handle_continue(:resolve_non_data_dependencies, state) do
    log(state, "resolving non data dependencies")

    case unresolved_non_data_fields(state.request) do
      [] ->
        raise "unreachable that none are unresolved"

      [{key, %{deps: []} = unresolved} | rest] ->
        log(state, "unresolved field #{inspect(key)} had no dependencies, resolving in place")

        case Request.resolve_field(%{}, unresolved) do
          {:ok, resolved} ->
            log(state, "#{key} successfully resolved")
            new_request = Map.put(state.request, key, resolved)
            new_state = Map.put(state, :request, new_request)
            GenServer.cast(state.engine_pid, {:updated_request, new_request})

            if Enum.empty?(rest) do
              log(state, "no more unresolved non data dependencies, moving on to strict check")
              {:noreply, new_state, {:continue, :strict_check}}
            else
              {:noreply, state, {:continue, :resolve_non_data_dependencies}}
            end

          {:error, error} ->
            log(state, "error when resolving #{key} #{inspect(error)}")
            {:stop, {:error, error}, state}
        end
    end
  end

  defp unresolved_non_data_fields(request) do
    request
    |> Map.delete(:data)
    |> Map.to_list()
    |> Enum.filter(fn {_, value} ->
      case value do
        %Request.UnresolvedField{} ->
          true

        _ ->
          false
      end
    end)
  end

  defp can_strict_check?(request) do
    request
    |> unresolved_non_data_fields()
    |> Enum.empty?()
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, request: request}, message, level) do
    Logger.log(level, "#{request.name}: #{message}")
  end

  defp log(_, _, _) do
    false
  end
end
