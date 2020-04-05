defmodule Ash.Engine2.Request do
  alias Ash.Authorization.{Check, Clause}

  defmodule UnresolvedField do
    defstruct [:resolver, depends_on: [], can_use: [], data?: false]

    def data(dependencies, can_use \\ [], func) do
      %__MODULE__{
        resolver: func,
        depends_on: deps(dependencies),
        can_use: deps(can_use),
        data?: true
      }
    end

    def field(dependencies, can_use \\ [], func) do
      %__MODULE__{
        resolver: func,
        depends_on: deps(dependencies),
        can_use: deps(can_use),
        data?: false
      }
    end

    defp deps(deps) do
      deps
      |> List.wrap()
      |> Enum.map(fn dep -> [:root | List.wrap(dep)] end)
    end
  end

  defimpl Inspect, for: UnresolvedField do
    import Inspect.Algebra

    def inspect(field, opts) do
      data =
        if field.data? do
          "data! "
        else
          ""
        end

      concat([
        "#UnresolvedField<",
        data,
        "needs: ",
        to_doc(field.depends_on, opts),
        ", can_use: ",
        to_doc(field.can_use, opts),
        ">"
      ])
    end
  end

  defmodule ResolveError do
    defstruct [:error]
  end

  defstruct [
    :rules,
    :strict_check_complete?,
    :strict_access?,
    :resource,
    :changeset,
    :path,
    :action_type,
    :data,
    :resolve_when_fetch_only?,
    :name,
    :filter,
    :context
  ]

  def new(opts) do
    rule_filter =
      case opts[:filter] do
        %UnresolvedField{} ->
          nil

        other ->
          other
      end

    rules =
      Enum.map(opts[:rules] || [], fn {rule, fact} ->
        {rule,
         Ash.Authorization.Clause.new(
           opts[:relationship] || [],
           opts[:resource],
           fact,
           opts[:clause_source] || :root,
           rule_filter
         )}
      end)

    %__MODULE__{
      rules: rules,
      strict_access?: Keyword.get(opts, :strict_access?, true),
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: [:root | opts[:path] || []],
      action_type: opts[:action_type],
      data: opts[:data],
      resolve_when_fetch_only?: opts[:resolve_when_fetch_only?],
      filter: opts[:filter],
      name: opts[:name],
      context: opts[:context] || %{}
    }
  end

  def can_strict_check?(%__MODULE__{strict_check_complete?: true}), do: false

  def can_strict_check?(request) do
    request
    |> Map.from_struct()
    |> Enum.all?(fn {_key, value} ->
      !match?(%UnresolvedField{data?: false}, value)
    end)
  end

  def authorize_always(request) do
    clause = Clause.new(request.path, request.resource, {Check.Static, result: true}, :root)

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

  def resolve_fields(
        request,
        data,
        include_data? \\ false
      ) do
    request
    |> Map.from_struct()
    |> Enum.reduce(request, fn {key, value}, request ->
      case value do
        %UnresolvedField{depends_on: dependencies, data?: data?}
        when include_data? or data? == false ->
          if dependencies_met?(data, dependencies) do
            case resolve_field(data, request, value) do
              {:ok, new_value} ->
                Map.put(request, key, new_value)

              %UnresolvedField{} = new_field ->
                Map.put(request, key, new_field)

              {:error, error} ->
                Map.put(request, key, %ResolveError{error: error})
            end
          else
            request
          end

        _ ->
          request
      end
    end)
  end

  def data_resolved?(%__MODULE__{data: %UnresolvedField{}}), do: false
  def data_resolved?(_), do: true

  defp resolve_field(data, request, %UnresolvedField{resolver: resolver} = unresolved) do
    context = resolver_context(data, unresolved)

    resolver.(request, unresolved, context)
  end

  def resolve_data(data, %{data: %UnresolvedField{resolver: resolver} = unresolved} = request) do
    context = resolver_context(data, unresolved)

    case resolver.(request, context) do
      {:ok, resolved} -> {:ok, Map.put(request, :data, resolved)}
      {:error, error} -> {:error, error}
    end
  end

  def resolve_data(_, request), do: {:ok, request}

  defp resolver_context(state, %{depends_on: depends_on, can_use: can_use}) do
    with_dependencies =
      Enum.reduce(depends_on, %{}, fn dependency, acc ->
        {:ok, value} = fetch_nested_value(state, dependency)
        put_nested_key(acc, dependency, value)
      end)

    Enum.reduce(can_use, with_dependencies, fn can_use, acc ->
      case fetch_nested_value(state, can_use) do
        {:ok, value} -> put_nested_key(acc, can_use, value)
        _ -> acc
      end
    end)
  end

  def all_dependencies_met?(request, state) do
    dependencies_met?(state, get_dependencies(request))
  end

  defp dependencies_met?(_state, []), do: true
  defp dependencies_met?(_state, nil), do: true

  defp dependencies_met?(state, dependencies) do
    Enum.all?(dependencies, fn dependency ->
      case fetch_nested_value(state, dependency) do
        {:ok, _} -> true
        _ -> false
      end
    end)
  end

  def depends_on?(request, other_request) do
    dependencies = get_dependencies(request)

    Enum.any?(dependencies, fn dep ->
      List.starts_with?(dep, other_request.path)
    end)
  end

  defp get_dependencies(request) do
    request
    |> Map.from_struct()
    |> Enum.flat_map(fn {_key, value} ->
      case value do
        %UnresolvedField{depends_on: values} ->
          values

        _ ->
          []
      end
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

  defp fetch_nested_value(state, [key]) when is_map(state) do
    Map.fetch(state, key)
  end

  defp fetch_nested_value(state, [key | rest]) when is_map(state) do
    case Map.fetch(state, key) do
      {:ok, value} -> fetch_nested_value(value, rest)
      :error -> :error
    end
  end

  defp fetch_nested_value(state, key) when is_map(state) do
    Map.fetch(state, key)
  end
end
