defmodule Ash.Engine.Request do
  defmodule UnresolvedField do
    # TODO: Add some kind of optional dependency?
    defstruct [:resolver, deps: [], optional_deps: [], data?: false]

    def new(dependencies, optional_deps, func) do
      %__MODULE__{
        resolver: func,
        deps: deps(dependencies),
        optional_deps: deps(optional_deps)
      }
    end

    defp deps(deps) do
      deps
      |> List.wrap()
      |> Enum.map(fn dep -> List.wrap(dep) end)
    end
  end

  defimpl Inspect, for: UnresolvedField do
    import Inspect.Algebra

    def inspect(field, opts) do
      concat([
        "#UnresolvedField<",
        to_doc(field.deps, opts),
        ">"
      ])
    end
  end

  defmodule ResolveError do
    defstruct [:error]
  end

  defstruct [
    :id,
    :error?,
    :resource,
    :changeset,
    :path,
    :action_type,
    :action,
    :data,
    :name,
    :api,
    :query,
    :write_to_data?,
    :verbose?,
    :state,
    authorizer_state: %{},
    dependencies_request: [],
    dependencies_to_send: %{}
  ]

  require Logger

  def resolve(dependencies \\ [], optional_dependencies \\ [], func) do
    UnresolvedField.new(dependencies, optional_dependencies, func)
  end

  def new(opts) do
    query =
      case opts[:query] do
        %UnresolvedField{} = query ->
          query

        %Ash.Query{} = query ->
          query

        nil ->
          nil

        other ->
          raise "Got a weird thing #{inspect(other)}"
      end

    id = Ecto.UUID.generate()

    data =
      case opts[:data] do
        %UnresolvedField{} = unresolved ->
          %{unresolved | data?: true}

        other ->
          other
      end

    %__MODULE__{
      id: id,
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: List.wrap(opts[:path]),
      action_type: opts[:action_type],
      action: opts[:action],
      data: data,
      query: query,
      api: opts[:api],
      name: opts[:name],
      state: :strict_check,
      verbose?: opts[:verbose?] || false,
      write_to_data?: Keyword.get(opts, :write_to_data?, true)
    }
  end

  def next(%{state: :strict_check} = request) do
    case Ash.authorizers(request.resource) do
      [] ->
        log(request, "No authorizers found, skipping strict check")
        {:continue, %{request | state: :fetch_data}}

      authorizers ->
        case strict_check(authorizers, request) do
          {:ok, new_request, []} ->
            log(new_request, "Strict check complete")
            {:continue, %{new_request | state: :fetch_data}}

          {:ok, new_request, dependencies} ->
            log(new_request, "Strict check incomplete, waiting on dependencies")
            {:waiting, new_request, dependencies}

          {:error, error} ->
            log(request, "Strict checking failed")
            {:error, error, request}
        end
    end
  end

  def next(%{state: :strict_check} = request) do
    case try_resolve_local(request, [:data], false) do
      {:skipped, _, _} ->
        raise "unreachable!"

      {:ok, state, []} ->
        {:continue, %{request | state: :check}}

      {:ok, state, _} ->
        {:noreply, state}

      {:error, error} ->
        {:stop, {:error, error, state.request}, state}
    end
  end

  defp strict_check(authorizers, request) do
    Enum.reduce_while(authorizers, {:ok, request, true}, fn authorizer,
                                                            {:ok, request, waiting_for} ->
      case do_strict_check(authorizer, request) do
        {:ok, new_request} ->
          log(new_request, "strict check succeeded for #{inspect(authorizer)}")
          {:cont, {:ok, new_request, waiting_for}}

        {:waiting, new_request, new_deps} ->
          log(
            new_request,
            "waiting on dependencies: #{inspect(new_deps)} for #{inspect(authorizer)}"
          )

          {:cont, {:ok, new_request, new_deps ++ waiting_for}}

        {:error, error} ->
          log(request, "strict check failed for #{inspect(authorizer)}: #{inspect(error)}")

          {:halt, {:error, error}}
      end
    end)
  end

  defp do_strict_check(authorizer, request) do
    case missing_strict_check_dependencies?(authorizer, request) do
      [] ->
        case strict_check_authorizer(authorizer, request) do
          :authorized ->
            {:ok, set_authorizer_state(request, authorizer, :authorized)}

          {:continue, authorizer_state} ->
            {:ok, set_authorizer_state(request, authorizer, authorizer_state)}

          {:error, error} ->
            {:error, error}
        end

      deps ->
        deps =
          Enum.map(deps, fn dep ->
            request.path ++ [dep]
          end)

        case try_resolve(request, deps) do
          {:ok, new_request, []} ->
            do_strict_check(authorizer, new_request)

          {:ok, new_request, waiting_for} ->
            {:waiting, new_request, waiting_for}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp missing_strict_check_dependencies?(authorizer, request) do
    authorizer
    |> Authorizer.strict_check_context(authorizer_state(request, authorizer))
    |> Enum.filter(fn dependency ->
      match?(%UnresolvedField{}, Map.get(request, dependency))
    end)
  end

  defp missing_check_dependencies(authorizer, request) do
    authorizer
    |> Authorizer.check_context(authorizer_state(request, authorizer))
    |> Enum.filter(fn dependency ->
      match?(%UnresolvedField{}, Map.get(request, dependency))
    end)
  end

  defp strict_check_authorizer(authorizer, state) do
    log(state, "strict checking for #{inspect(authorizer)}")

    authorizer_state = authorizer_state(state, authorizer)

    keys = Authorizer.strict_check_context(authorizer, authorizer_state)

    Authorizer.strict_check(authorizer, authorizer_state, Map.take(state.request, keys))
  end

  defp check_authorizer(authorizer, state) do
    log(state, "checking for #{inspect(authorizer)}")

    authorizer_state = authorizer_state(state, authorizer)

    keys = Authorizer.check_context(authorizer, authorizer_state)

    Authorizer.check(authorizer, authorizer_state, Map.take(state.request, keys))
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

  def validate_unique_paths(requests) do
    requests
    |> Enum.group_by(& &1.path)
    |> Enum.filter(fn {_path, value} ->
      Enum.count(value, & &1.write_to_data?) > 1
    end)
    |> case do
      [] ->
        :ok

      invalid_paths ->
        invalid_paths = Enum.map(invalid_paths, &elem(&1, 0))

        {:error, invalid_paths}
    end
  end

  def build_dependencies(requests) do
    result =
      Enum.reduce_while(requests, {:ok, []}, fn request, {:ok, new_requests} ->
        case do_build_dependencies(request, requests) do
          {:ok, new_request} -> {:cont, {:ok, [new_request | new_requests]}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)

    case result do
      {:ok, requests} -> {:ok, Enum.reverse(requests)}
      other -> other
    end
  end

  defp do_build_dependencies(request, requests, trail \\ []) do
    request
    |> Map.from_struct()
    |> Enum.reduce_while({:ok, request}, fn
      {key, %UnresolvedField{deps: deps} = unresolved}, {:ok, request} ->
        case expand_deps(deps, requests, trail) do
          {:error, error} ->
            {:halt, {:error, error}}

          {:ok, new_deps} ->
            {:cont, {:ok, Map.put(request, key, %{unresolved | deps: Enum.uniq(new_deps)})}}
        end

      _, {:ok, request} ->
        {:cont, {:ok, request}}
    end)
  end

  defp expand_deps([], _, _), do: {:ok, []}

  defp expand_deps(deps, requests, trail) do
    Enum.reduce_while(deps, {:ok, []}, fn dep, {:ok, all_new_deps} ->
      case do_expand_dep(dep, requests, trail) do
        {:ok, new_deps} -> {:cont, {:ok, all_new_deps ++ new_deps}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp do_expand_dep(dep, requests, trail) do
    if dep in trail do
      {:error, {:circular, dep}}
    else
      # TODO: this is inneficient
      request_path = :lists.droplast(dep)
      request_key = List.last(dep)

      case Enum.find(requests, &(&1.path == request_path)) do
        nil ->
          {:error, {:impossible, dep}}

        %{^request_key => %UnresolvedField{deps: nested_deps}} ->
          case expand_deps(nested_deps, requests, [dep | trail]) do
            {:ok, new_deps} -> {:ok, [dep | new_deps]}
            other -> other
          end

        _ ->
          {:ok, [dep]}
      end
    end
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, name: name}, message, level) do
    Logger.log(level, "#{name}: #{message}")
  end

  defp log(_, _, _) do
    false
  end
end
