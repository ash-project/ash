defmodule Ash.Engine.Request do
  alias Ash.Authorization.{Check, Clause}

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
    :rules,
    :strict_access?,
    :resource,
    :changeset,
    :path,
    :action_type,
    :data,
    :resolve_when_fetch_only?,
    :name,
    :query,
    :context,
    :write_to_data?,
    strict_check_complete?: false,
    check_complete?: false,
    prepared?: false
  ]

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

    clause_id =
      if opts[:action_type] == :read do
        nil
      else
        id
      end

    rules =
      Enum.map(opts[:rules] || [], fn {rule, fact} ->
        {rule,
         Ash.Authorization.Clause.new(
           opts[:resource],
           fact,
           opts[:action],
           Map.get(query || %{}, :filter),
           clause_id
         )}
      end)

    data =
      case opts[:data] do
        %UnresolvedField{} = unresolved ->
          %{unresolved | data?: true}

        other ->
          other
      end

    %__MODULE__{
      id: id,
      rules: rules,
      strict_access?: Keyword.get(opts, :strict_access?, true),
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: List.wrap(opts[:path]),
      action_type: opts[:action_type],
      data: data,
      resolve_when_fetch_only?: opts[:resolve_when_fetch_only?],
      query: query,
      name: opts[:name],
      context: opts[:context] || %{},
      write_to_data?: Keyword.get(opts, :write_to_data?, true)
    }
  end

  def can_strict_check(%__MODULE__{strict_check_complete?: true}, _state), do: false

  def can_strict_check(request, state) do
    all_dependencies_met?(request, state, false)
  end

  def authorize_always(request) do
    filter =
      case request.query do
        %UnresolvedField{} ->
          nil

        %Ash.Query{filter: filter} ->
          filter

        nil ->
          nil
      end

    clause = Clause.new(request.resource, {Check.Static, result: true}, request.action, filter)

    %{request | rules: [authorize_if: clause]}
  end

  def errors(request) do
    request
    |> Map.from_struct()
    |> Enum.filter(fn {_key, value} ->
      match?(%ResolveError{}, value)
    end)
    |> Enum.into(%{})
  end

  def data_resolved?(%__MODULE__{data: %UnresolvedField{}}), do: false
  def data_resolved?(_), do: true

  def resolve_field(data, %UnresolvedField{resolver: resolver} = unresolved) do
    context = resolver_context(data, unresolved)

    resolver.(context)
  end

  def resolve_data(data, %{data: %UnresolvedField{resolver: resolver} = unresolved} = request) do
    context = resolver_context(data, unresolved)

    case resolver.(context) do
      {:ok, resolved} -> {:ok, Map.put(request, :data, resolved)}
      {:error, error} -> {:error, error}
    end
  rescue
    e ->
      if is_map(e) do
        {:error, Map.put(e, :__stacktrace__, __STACKTRACE__)}
      else
        {:error, e}
      end
  end

  def resolve_data(_, request), do: {:ok, request}

  def contains_clause?(request, clause) do
    Enum.any?(request.rules, fn {_step, request_clause} ->
      clause == request_clause
    end)
  end

  def put_request(state, request) do
    put_nested_key(state, request.path, request)
  end

  def fetch_request_state(state, request) do
    fetch_nested_value(state, request.path)
  end

  defp resolver_context(state, %{deps: depends_on, optional_deps: optional_deps}) do
    with_dependencies =
      Enum.reduce(depends_on, %{}, fn dependency, acc ->
        {:ok, value} = fetch_nested_value(state, dependency)

        put_nested_key(acc, dependency, value)
      end)

    Enum.reduce(optional_deps, with_dependencies, fn optional_dep, acc ->
      case fetch_nested_value(state, optional_dep) do
        {:ok, value} -> put_nested_key(acc, optional_dep, value)
        _ -> acc
      end
    end)
  end

  def all_dependencies_met?(request, state, data? \\ true) do
    dependencies_met?(state, get_dependencies(request, data?), data?)
  end

  def dependencies_met?(state, deps, data? \\ true)
  def dependencies_met?(_state, [], _), do: true
  def dependencies_met?(_state, nil, _), do: true

  def dependencies_met?(state, dependencies, data?) do
    Enum.all?(dependencies, fn dependency ->
      case fetch_nested_value(state, dependency) do
        {:ok, %UnresolvedField{deps: nested_dependencies, data?: dep_is_data?}} ->
          if dep_is_data? and not data? do
            false
          else
            dependencies_met?(state, nested_dependencies, data?)
          end

        {:ok, _} ->
          true

        _ ->
          false
      end
    end)
  end

  def depends_on?(request, other_request) do
    dependencies = get_dependencies(request)

    Enum.any?(dependencies, fn dep ->
      List.starts_with?(dep, other_request.path)
    end)
  end

  def fetch_nested_value(state, [key]) when is_map(state) do
    Map.fetch(state, key)
  end

  def fetch_nested_value(%UnresolvedField{}, _), do: :error

  def fetch_nested_value(state, [key | rest]) when is_map(state) do
    case Map.fetch(state, key) do
      {:ok, value} -> fetch_nested_value(value, rest)
      :error -> :error
    end
  end

  def fetch_nested_value(state, key) when is_map(state) do
    Map.fetch(state, key)
  end

  # Debugging utility
  def deps_report(requests) when is_list(requests) do
    Enum.map_join(requests, &deps_report/1)
  end

  def deps_report(request) do
    header = "#{request.name}: \n"

    body =
      request
      |> Map.from_struct()
      |> Enum.filter(&match?({_, %UnresolvedField{}}, &1))
      |> Enum.map_join("\n", fn {key, value} ->
        "  #{key}: #{inspect(value)}"
      end)

    header <> body <> "\n"
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
          {:error, error} -> {:halt, {:error, request.path, error}}
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

  defp get_dependencies(request, data? \\ true) do
    keys_to_drop =
      if data? do
        []
      else
        [:data]
      end

    request
    |> Map.from_struct()
    |> Map.drop(keys_to_drop)
    |> Enum.flat_map(fn
      {_key, %UnresolvedField{deps: values}} ->
        values

      _ ->
        []
    end)
    |> Enum.uniq()
  end

  defp put_nested_key(state, [key], value) do
    Map.put(state, key, value)
  end

  defp put_nested_key(state, [key | rest], value) do
    case Map.fetch(state, key) do
      {:ok, nested_state} when is_map(nested_state) ->
        Map.put(state, key, put_nested_key(nested_state, rest, value))

      :error ->
        Map.put(state, key, put_nested_key(%{}, rest, value))
    end
  end

  defp put_nested_key(state, key, value) do
    Map.put(state, key, value)
  end
end
