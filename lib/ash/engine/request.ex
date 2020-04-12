defmodule Ash.Engine.Request do
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
      |> Enum.map(fn dep -> List.wrap(dep) end)
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
    :id,
    :error?,
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
    filter =
      case opts[:filter] do
        %UnresolvedField{} ->
          nil

        %Ash.Filter{} = filter ->
          filter

        nil ->
          nil

        other ->
          Ash.Filter.parse(opts[:resource], other)
      end

    rules =
      Enum.map(opts[:rules] || [], fn {rule, fact} ->
        {rule,
         Ash.Authorization.Clause.new(
           opts[:resource],
           fact,
           filter
         )}
      end)

    %__MODULE__{
      id: Ecto.UUID.generate(),
      rules: rules,
      strict_access?: Keyword.get(opts, :strict_access?, true),
      resource: opts[:resource],
      changeset: opts[:changeset],
      path: List.wrap(opts[:path]),
      action_type: opts[:action_type],
      data: opts[:data],
      resolve_when_fetch_only?: opts[:resolve_when_fetch_only?],
      filter: filter,
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
    clause = Clause.new(request.resource, {Check.Static, result: true})

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

  # def resolve_fields(
  #       request,
  #       data,
  #       include_data? \\ false
  #     ) do
  #   request
  #   |> Map.from_struct()
  #   |> Enum.reduce(request, fn {key, value}, request ->
  #     case value do
  #       %UnresolvedField{depends_on: dependencies, data?: data?}
  #       when include_data? or data? == false ->
  #         if dependencies_met?(data, dependencies) do
  #           case resolve_field(data, value) do
  #             {:ok, new_value} ->
  #               Map.put(request, key, new_value)

  #             %UnresolvedField{} = new_field ->
  #               Map.put(request, key, new_field)

  #             {:error, error} ->
  #               Map.put(request, key, %ResolveError{error: error})
  #           end
  #         else
  #           request
  #         end

  #       _ ->
  #         request
  #     end
  #   end)
  # end

  def data_resolved?(%__MODULE__{data: %UnresolvedField{}}), do: false
  def data_resolved?(_), do: true

  def resolve_field(data, %UnresolvedField{resolver: resolver} = unresolved) do
    context = resolver_context(data, unresolved)

    resolver.(context)
  end

  def resolve_data(data, %{data: %UnresolvedField{resolver: resolver} = unresolved} = request) do
    # {new_data = resolve_
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

  def dependencies_met?(state, dependencies, sources \\ [])
  def dependencies_met?(_state, [], _sources), do: {true, []}
  def dependencies_met?(_state, nil, _sources), do: {true, []}

  def dependencies_met?(state, dependencies, sources) do
    Enum.reduce(dependencies, {true, []}, fn
      _, false ->
        false

      dependency, {true, if_resolved} ->
        if dependency in sources do
          # Prevent infinite loop on co-dependent requests
          # Does it make sense to have to do this?
          false
        else
          case fetch_nested_value(state, dependency) do
            {:ok, %UnresolvedField{depends_on: nested_dependencies}} ->
              case dependencies_met?(state, nested_dependencies, [dependency | sources]) do
                {true, nested_if_resolved} ->
                  {true, [dependency | if_resolved] ++ nested_if_resolved}

                false ->
                  false
              end

            {:ok, _} ->
              {true, if_resolved}

            _ ->
              false
          end
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
end
