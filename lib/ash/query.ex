defmodule Ash.Query do
  defstruct [
    :api,
    :resource,
    :filter,
    :data_layer_query,
    side_load: [],
    sort: [],
    limit: nil,
    offset: 0,
    errors: [],
    valid?: true
  ]

  @type t :: %__MODULE__{}

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(query, opts) do
      error_doc =
        if Enum.empty?(query.errors) do
          empty()
        else
          concat("errors: ", to_doc(query.errors, opts))
        end

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", Ash.name(query.resource)),
          concat("filter: ", to_doc(query.filter, opts)),
          concat("sort: ", to_doc(query.sort, opts)),
          concat("limit: ", to_doc(query.limit, opts)),
          concat("offset: ", to_doc(query.offset, opts)),
          concat("side_load: ", to_doc(query.side_load, opts)),
          error_doc
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end
  end

  @doc false
  def new(api, resource) when is_atom(api) and is_atom(resource) do
    case api.get_resource(resource) do
      {:ok, resource} ->
        %__MODULE__{
          api: api,
          filter: Ash.Filter.parse(resource, [], api),
          resource: resource
        }
        |> set_data_layer_query()

      :error ->
        %__MODULE__{
          api: api,
          filter: Ash.Filter.parse(resource, [], api),
          resource: resource
        }
        |> add_error(:resource, "does not exist")
    end
  end

  def limit(query, nil), do: query

  def limit(query, limit) when is_integer(limit) do
    query
    |> Map.put(:limit, max(0, limit))
    |> set_data_layer_query()
  end

  def limit(query, limit) do
    add_error(query, :offset, Ash.Error.InvalidLimit.exception(limit: limit))
  end

  def offset(query, nil), do: query

  def offset(query, offset) when is_integer(offset) do
    query
    |> Map.put(:offset, max(0, offset))
    |> set_data_layer_query()
  end

  def offset(query, offset) do
    add_error(query, :offset, Ash.Error.InvalidOffset.exception(offset: offset))
  end

  def side_load(query, statement) do
    with sanitized_statement <- sanitize_side_loads(statement),
         :ok <- validate_side_load(query.resource, sanitized_statement),
         new_side_loads <- merge_side_load(query.side_load, sanitized_statement) do
      %{query | side_load: new_side_loads}
    else
      {:error, errors} ->
        Enum.reduce(errors, query, &add_error(&2, :side_load, &1))
    end
  end

  def validate_side_load(resource, side_loads, path \\ []) do
    case do_validate_side_load(resource, side_loads, path) do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  def do_validate_side_load(_resource, %Ash.Query{} = query, path) do
    case query.errors do
      [] ->
        []

      _errors ->
        [
          {:error,
           Ash.Error.SideLoad.InvalidQuery.exception(
             query: query,
             side_load_path: Enum.reverse(path)
           )}
        ]
    end
  end

  def do_validate_side_load(resource, side_loads, path) when is_list(side_loads) do
    side_loads
    |> List.wrap()
    |> Enum.flat_map(fn
      {_key, %Ash.Query{}} ->
        []

      {key, value} ->
        case Ash.relationship(resource, key) do
          nil ->
            [
              {:error,
               Ash.Error.SideLoad.NoSuchRelationship.exception(
                 resource: resource,
                 relationship: key,
                 side_load_path: Enum.reverse(path)
               )}
            ]

          %{destination: relationship_resource} = relationship ->
            case value do
              %__MODULE__{resource: query_resource} = destination_query
              when query_resource != relationship_resource ->
                [
                  Ash.Error.SideLoad.InvalidQuery.exception(
                    resource: resource,
                    relationship: key,
                    query: destination_query,
                    side_load_path: Enum.reverse(path)
                  )
                ]

              other ->
                do_validate_side_load(relationship.destination, other, [key | path])
            end
        end
    end)
  end

  def merge_side_load([], right), do: sanitize_side_loads(right)
  def merge_side_load(left, []), do: sanitize_side_loads(left)

  def merge_side_load(
        %__MODULE__{side_load: left_side_loads},
        %__MODULE__{side_load: right_side_loads} = query
      ) do
    %{query | side_load: merge_side_load(left_side_loads, right_side_loads)}
  end

  def merge_side_load(%__MODULE__{} = query, right) when is_list(right) do
    Ash.Query.side_load(query, right)
  end

  def merge_side_load(left, %Ash.Query{} = query) when is_list(left) do
    Ash.Query.side_load(query, left)
  end

  def merge_side_load(left, right) when is_atom(left), do: merge_side_load([{left, []}], right)
  def merge_side_load(left, right) when is_atom(right), do: merge_side_load(left, [{right, []}])

  def merge_side_load(left, right) when is_list(left) and is_list(right) do
    right
    |> sanitize_side_loads()
    |> Enum.reduce(sanitize_side_loads(left), fn {rel, rest}, acc ->
      Keyword.update(acc, rel, rest, &merge_side_load(&1, rest))
    end)
  end

  defp sanitize_side_loads(side_load) when is_atom(side_load), do: {side_load, []}

  defp sanitize_side_loads(%Ash.Query{} = query) do
    Map.update!(query, :side_load, &sanitize_side_loads/1)
  end

  defp sanitize_side_loads(side_loads) do
    Enum.map(side_loads, fn
      {key, value} ->
        {key, sanitize_side_loads(value)}

      side_load_part ->
        cond do
          is_atom(side_load_part) -> {side_load_part, []}
          is_list(side_load_part) -> sanitize_side_loads(side_load_part)
          true -> side_load_part
        end
    end)
  end

  def filter(query, %Ash.Filter{} = filter) do
    new_filter =
      case query.filter do
        nil ->
          filter

        existing_filter ->
          Ash.Filter.add_to_filter(existing_filter, filter)
      end

    new_filter.errors
    |> Enum.reduce(query, &add_error(&2, :filter, &1))
    |> Map.put(:filter, new_filter)
    |> set_data_layer_query()
  end

  def filter(query, statement) do
    filter =
      if query.filter do
        Ash.Filter.add_to_filter(query.filter, statement)
      else
        Ash.Filter.parse(query.resource, statement, query.api)
      end

    filter.errors
    |> Enum.reduce(query, &add_error(&2, :filter, &1))
    |> Map.put(:filter, filter)
    |> set_data_layer_query()
  end

  def reject(query, statement) when is_list(statement) do
    filter(query, not: statement)
  end

  def reject(query, %Ash.Filter{} = filter) do
    case query.filter do
      nil ->
        new_filter =
          query.resource
          |> Ash.Filter.parse([], query.api)
          |> Map.put(:not, filter)

        query
        |> Map.put(:filter, new_filter)
        |> set_data_layer_query()

      existing_filter ->
        new_filter_not =
          case existing_filter.not do
            nil ->
              filter

            existing_not_filter ->
              %{existing_not_filter | ands: [filter | existing_not_filter.ands]}
          end

        new_filter = %{existing_filter | not: new_filter_not}

        query
        |> Map.put(:filter, new_filter)
        |> set_data_layer_query()
    end
  end

  def sort(query, sorts) when is_list(sorts) do
    sorts
    |> Enum.reduce(query, fn
      {sort, direction}, query ->
        %{query | sort: query.sort ++ [{sort, direction}]}

      sort, query ->
        %{query | sort: query.sort ++ [{sort, :asc}]}
    end)
    |> validate_sort()
    |> set_data_layer_query()
  end

  defp validate_sort(%{resource: resource, sort: sort} = query) do
    case Ash.Actions.Sort.process(resource, sort) do
      {:ok, new_sort} -> %{query | sort: new_sort}
      {:error, error} -> add_error(query, :sort, error)
    end
  end

  def unset(query, key) when key in [:api, :resource] do
    add_error(query, key, "Cannot be unset")
  end

  def unset(query, key) do
    struct(query, [{key, Map.get(%__MODULE__{}, key)}])
  end

  defp add_error(query, key, message) do
    %{
      query
      | errors: [Map.put(Ash.to_ash_error(message), :path, key) | query.errors],
        valid?: false
    }
  end

  defp set_data_layer_query(query) do
    case data_layer_query(query) do
      {:ok, data_layer_query} -> %{query | data_layer_query: data_layer_query}
      {:error, error} -> add_error(query, :data_layer_query, error)
    end
  end

  @doc false
  def data_layer_query(%{resource: resource} = ash_query, opts \\ []) do
    query = Ash.DataLayer.resource_to_query(resource)

    with {:ok, query} <- Ash.DataLayer.sort(query, ash_query.sort, resource),
         {:ok, query} <- maybe_filter(query, ash_query, opts) do
      {:ok, query}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp maybe_filter(query, ash_query, opts) do
    case Ash.DataLayer.filter(query, ash_query.filter, ash_query.resource) do
      {:ok, filtered} ->
        if Keyword.get(opts, :only_validate_filter?, true) do
          {:ok, query}
        else
          {:ok, filtered}
        end

      {:error, error} ->
        {:error, error}
    end
  end
end
