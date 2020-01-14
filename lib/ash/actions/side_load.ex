defmodule Ash.Actions.SideLoad do
  def requests(api, resource, side_load, path \\ [])
  def requests(_, _, [], _), do: {:ok, []}

  def requests(api, resource, side_load, path) do
    # TODO: return authorizations here.
    Enum.reduce(side_load, {:ok, []}, fn
      _, {:error, error} ->
        {:error, error}

      {key, true}, {:ok, acc} ->
        do_requests(api, resource, key, [], path, acc)

      {key, further}, {:ok, acc} ->
        do_requests(api, resource, key, further, path, acc)

      key, {:ok, acc} ->
        do_requests(api, resource, key, [], path, acc)
    end)
  end

  def merge([], right), do: right
  def merge(left, []), do: left
  def merge(left, right) when is_atom(left), do: merge([{left, []}], right)
  def merge(left, right) when is_atom(right), do: merge(left, [{right, []}])

  def merge(left, right) when is_list(left) and is_list(right) do
    right
    |> sanitize_side_loads()
    |> Enum.reduce(sanitize_side_loads(left), fn {rel, rest}, acc ->
      case Keyword.fetch(acc, rel) do
        {:ok, value} -> merge(value, rest)
        :error -> Keyword.put(acc, rel, rest)
      end
    end)
  end

  def side_load(api, resource, data, side_load) do
    requests = requests(api, resource, side_load)

    case Ash.Engine.run(nil, requests, state: %{data: data}) do
      {:ok, state} ->
        case data do
          nil ->
            {:ok, nil}

          [] ->
            {:ok, []}

          data when is_list(data) ->
            {:ok, attach_side_loads(data, state)}

          data ->
            {:ok, List.first(attach_side_loads([data], state))}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def attach_side_loads(%Ash.Actions.Paginator{results: results} = paginator, state) do
    %{paginator | results: attach_side_loads(results, state)}
  end

  def attach_side_loads([%resource{} | _] = data, %{include: includes}) when is_list(data) do
    includes
    |> Enum.sort_by(fn {key, _value} ->
      length(key)
    end)
    |> Enum.reduce(data, fn {key, value}, data ->
      last_relationship = last_relationship!(resource, key)

      {values, default} =
        case last_relationship.cardinality do
          :many ->
            values = Enum.group_by(value, &Map.get(&1, last_relationship.destination_field))
            {values, []}

          :one ->
            values =
              Enum.into(value, %{}, fn item ->
                {Map.get(item, last_relationship.destination_field), item}
              end)

            {values, nil}
        end

      add_include_to_data(data, key, last_relationship, values, default)
    end)
  end

  def attach_side_loads(data, state) when not is_list(data) do
    [data]
    |> attach_side_loads(state)
    |> List.first()
  end

  def attach_side_loads(data, _) do
    data
  end

  defp add_include_to_data(data, path, last_relationship, values, default)
       when not is_list(data) do
    [data]
    |> add_include_to_data(path, last_relationship, values, default)
    |> List.first()
  end

  defp add_include_to_data(data, [_key], last_relationship, values, default) do
    Enum.map(data, fn item ->
      source_value = Map.get(item, last_relationship.source_field)
      related_value = Map.get(values, source_value, default)

      Map.put(item, last_relationship.name, related_value)
    end)
  end

  defp add_include_to_data(data, [key | rest], last_relationship, values, default) do
    Enum.map(data, fn item ->
      Map.update!(item, key, fn related ->
        add_include_to_data(List.wrap(related), rest, last_relationship, values, default)
      end)
    end)
  end

  defp last_relationship!(resource, [last]) do
    Ash.relationship(resource, last) || raise "Assumption Failed"
  end

  defp last_relationship!(resource, [first | rest]) do
    relationship = Ash.relationship(resource, first) || raise "Assumption Failed"

    last_relationship!(relationship.destination, rest)
  end

  defp do_requests(api, resource, key, further, path, acc) do
    with {:rel, relationship} when not is_nil(relationship) <-
           {:rel, Ash.relationship(resource, key)},
         nested_path <- path ++ [relationship],
         {:ok, requests} <-
           requests(api, relationship.destination, further, nested_path) do
      default_read =
        Ash.primary_action(resource, :read) ||
          raise "Must set default read for #{inspect(resource)}"

      dependencies =
        if path == [] do
          [[:data]]
        else
          [[:data], [:include, Enum.map(path, &Map.get(&1, :name))]]
        end

      source =
        nested_path
        |> Enum.reverse()
        |> Enum.map_join(".", &Map.get(&1, :name))

      request =
        Ash.Engine.Request.new(
          action_type: :read,
          resource: resource,
          rules: default_read.rules,
          source: "side_load #{source}",
          api: api,
          state_key: [:include, Enum.map(nested_path, &Map.get(&1, :name))],
          must_fetch?: true,
          dependencies: dependencies,
          filter: fn state ->
            filter = side_load_filter(relationship, state, path)

            Ash.Filter.parse(resource, filter, api)
          end,
          fetcher: fn
            _, %{data: data} = state ->
              if relationship_already_loaded?(data, relationship, path) do
                {:ok, data}
              else
                # Because we have the records, we can optimize the filter by nillifying the reverse relationship.
                # The reverse relationship is useful if you don't have the relationship keys for the related items (only pkeys)
                # or for doing many to many joins, but can be slower.
                filter =
                  side_load_filter(%{relationship | reverse_relationship: nil}, state, path)

                case api.read(relationship.destination, filter: filter, paginate: false) do
                  {:ok, %{results: results}} -> {:ok, results}
                  {:error, error} -> {:error, error}
                end
              end
          end
        )

      {:ok, [request | requests] ++ acc}
    else
      {:rel, nil} -> {:error, "no such relationship: #{key}"}
      {:error, error} -> {:error, error}
    end
  end

  defp relationship_already_loaded?(data, relationship, path) do
    Enum.all?(get_field(data, relationship.name, path), fn item ->
      not match?(%Ecto.Association.NotLoaded{}, item)
    end)
  end

  defp side_load_filter(_relationship, %{data: []}, _) do
    []
  end

  defp side_load_filter(
         %{reverse_relationship: nil, type: :many_to_many} = relationship,
         _,
         _
       ) do
    # TODO: Validate this is unreachable at compile time
    raise "require reverse relationship for #{inspect(relationship)}"
  end

  defp side_load_filter(%{reverse_relationship: nil} = relationship, %{data: data}, []) do
    [
      {relationship.destination_field,
       [in: Enum.map(data, &Map.get(&1, relationship.source_field))]}
    ]
  end

  defp side_load_filter(%{reverse_relationship: nil} = relationship, %{data: data}, prior_path) do
    prior_path_names = Enum.map(prior_path, &Map.get(&1, :name))
    source_fields = get_field(data, relationship.source_field, prior_path_names)

    case source_fields do
      [] ->
        [__impossible__: true]

      [field] ->
        [{relationship.destination_field, field}]

      fields ->
        [{relationship.destination_field, [in: fields]}]
    end
  end

  defp side_load_filter(relationship, %{data: data} = state, prior_path) do
    case reverse_relationship_path_and_values(relationship, data, prior_path) do
      {:ok, path, values} ->
        case values do
          [] ->
            [__impossible__: true]

          [one_pkey] ->
            put_nested_relationship(path, Map.to_list(one_pkey))

          pkeys ->
            put_nested_relationship(path, Enum.map(pkeys, &Map.to_list/1))
        end

      :error ->
        prior_path_names = Enum.map(prior_path, &Map.get(&1, :name))
        related_data = get_in(state, [:include, prior_path_names])

        related_keys = get_field(related_data, relationship.source_field, [])

        case related_keys do
          [] ->
            [__impossible__: true]

          [one_value] ->
            [{relationship.destination_field, one_value}]

          values ->
            [{relationship.destination_field, [in: values]}]
        end
    end
  end

  defp reverse_relationship_path_and_values(relationship, data, prior_path, acc \\ [])

  defp reverse_relationship_path_and_values(%{reverse_relationship: nil}, _, _, _) do
    :error
  end

  defp reverse_relationship_path_and_values(relationship, data, [], acc) do
    path = Enum.reverse([relationship.reverse_relationship | acc])
    pkey = Ash.primary_key(relationship.source)

    values = get_fields(data, pkey)

    {:ok, path, values}
  end

  defp reverse_relationship_path_and_values(relationship, data, [next_relationship | rest], acc) do
    reverse_relationship_path_and_values(next_relationship, data, rest, [
      relationship.reverse_relationship | acc
    ])
  end

  defp get_fields(data, fields, path \\ [])

  defp get_fields(data, fields, []) do
    data
    |> List.wrap()
    |> Enum.map(&Map.take(&1, fields))
    |> Enum.uniq()
  end

  defp get_fields(data, fields, [first | rest]) do
    data
    |> List.wrap()
    |> Enum.flat_map(fn item ->
      item
      |> Map.get(first)
      |> List.wrap()
    end)
    |> get_fields(fields, rest)
  end

  defp get_field(data, name, []) do
    data
    |> Enum.map(&Map.get(&1, name))
    |> Enum.uniq()
  end

  defp get_field(data, name, [first | rest]) do
    data
    |> Enum.flat_map(fn item ->
      item
      |> Map.get(first)
      |> List.wrap()
    end)
    |> get_field(name, rest)
  end

  defp put_nested_relationship([rel | rest], value) do
    [
      {rel, put_nested_relationship(rest, value)}
    ]
  end

  defp put_nested_relationship([], value) do
    value
  end

  defp sanitize_side_loads(side_loads) do
    Enum.map(side_loads, fn side_load_part ->
      if is_atom(side_load_part) do
        {side_load_part, []}
      else
        side_load_part
      end
    end)
  end
end
