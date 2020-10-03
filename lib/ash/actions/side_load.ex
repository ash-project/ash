defmodule Ash.Actions.SideLoad do
  @moduledoc false

  alias Ash.Engine
  alias Ash.Engine.Request

  def requests(
        query,
        use_data_for_filter? \\ true,
        root_data \\ [],
        root_query \\ nil,
        path \\ []
      )

  def requests(nil, _, _, _, _), do: []
  def requests(%{side_load: []}, _, _, _root_query, _), do: []

  def requests(
        %{side_load: side_loads} = query,
        use_data_for_filter?,
        root_data,
        root_query,
        path
      ) do
    root_query = root_query || query

    side_loads
    |> List.wrap()
    |> Enum.flat_map(fn {relationship, further} ->
      relationship = Ash.Resource.relationship(query.resource, relationship)

      related_query =
        case further do
          %Ash.Query{} = query ->
            Ash.Query.set_api(query, root_query.api)

          further ->
            relationship.destination
            |> Ash.Query.new(root_query.api)
            |> Ash.Query.load(further)
        end

      new_path = [relationship | path]

      requests(related_query, use_data_for_filter?, root_data, root_query, new_path) ++
        do_requests(
          relationship,
          related_query,
          path,
          root_query,
          root_data,
          use_data_for_filter?
        )
    end)
  end

  def attach_side_loads([%resource{} | _] = data, %{side_load: side_loads})
      when is_list(data) do
    side_loads
    |> Enum.sort_by(fn {key, _value} ->
      last_relationship = last_relationship!(resource, key)
      # We want to do `many_to_many` last, so we know we've
      # done their join assocs first. Pretty hacky
      {length(key), last_relationship.type == :many_to_many}
    end)
    |> Enum.reduce(data, fn {key, value}, data ->
      last_relationship = last_relationship!(resource, key)
      lead_path = :lists.droplast(key)

      case last_relationship do
        %{type: :many_to_many} ->
          attach_many_to_many_side_loads(
            data,
            lead_path,
            last_relationship,
            side_loads,
            value
          )

        %{cardinality: :many} ->
          attach_to_many_side_loads(value, last_relationship, data, lead_path)

        %{cardinality: :one} ->
          attach_to_one_side_loads(value, last_relationship, data, lead_path)
      end
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

  defp attach_to_many_side_loads(value, last_relationship, data, lead_path) do
    values = Enum.group_by(value, &Map.get(&1, last_relationship.destination_field))

    map_or_update(data, lead_path, fn record ->
      source_key = Map.get(record, last_relationship.source_field)
      related_records = Map.get(values, source_key, [])
      Map.put(record, last_relationship.name, related_records)
    end)
  end

  defp attach_to_one_side_loads(value, last_relationship, data, lead_path) do
    values =
      Enum.into(value, %{}, fn item ->
        {Map.get(item, last_relationship.destination_field), item}
      end)

    map_or_update(data, lead_path, fn record ->
      source_key = Map.get(record, last_relationship.source_field)
      related_record = Map.get(values, source_key)
      Map.put(record, last_relationship.name, related_record)
    end)
  end

  defp attach_many_to_many_side_loads(data, lead_path, last_relationship, side_loads, value) do
    join_path = lead_path ++ [last_relationship.join_relationship]

    join_data =
      side_loads
      |> Map.get(join_path, [])

    map_or_update(data, lead_path, fn record ->
      source_value = Map.get(record, last_relationship.source_field)

      join_values =
        join_data
        |> Enum.filter(fn join_row ->
          Map.get(join_row, last_relationship.source_field_on_join_table) ==
            source_value
        end)
        |> Enum.map(&Map.get(&1, last_relationship.destination_field_on_join_table))

      related_records =
        value
        |> Enum.filter(fn value ->
          destination_value = Map.get(value, last_relationship.destination_field)

          destination_value in join_values
        end)

      Map.put(record, last_relationship.name, related_records)
    end)
  end

  defp map_or_update(nil, _, _), do: nil
  defp map_or_update(record, [], func) when not is_list(record), do: func.(record)

  defp map_or_update(records, [], func) do
    Enum.map(records, fn record ->
      if record do
        func.(record)
      else
        nil
      end
    end)
  end

  defp map_or_update(records, [path | tail], func) do
    map_or_update(records, [], fn record ->
      Map.update!(record, path, &map_or_update(&1, tail, func))
    end)
  end

  defp last_relationship!(resource, [last]) do
    Ash.Resource.relationship(resource, last) || raise "Assumption Failed"
  end

  defp last_relationship!(resource, [first | rest]) do
    relationship = Ash.Resource.relationship(resource, first) || raise "Assumption Failed"

    last_relationship!(relationship.destination, rest)
  end

  defp do_requests(relationship, related_query, path, root_query, root_data, use_data_for_filter?) do
    side_load_request =
      side_load_request(
        relationship,
        related_query,
        root_query,
        path,
        root_data,
        use_data_for_filter?
      )

    case relationship.type do
      :many_to_many ->
        case join_assoc_request(
               relationship,
               related_query,
               root_query,
               path,
               root_data,
               use_data_for_filter?
             ) do
          nil ->
            [side_load_request]

          request ->
            [side_load_request, request]
        end

      _ ->
        [side_load_request]
    end
  end

  defp side_load_request(
         relationship,
         related_query,
         root_query,
         path,
         root_data,
         use_data_for_filter?
       ) do
    request_path = [
      :side_load,
      Enum.reverse(Enum.map([relationship | path], &Map.get(&1, :name)))
    ]

    dependencies =
      case path do
        [] ->
          [request_path ++ [:authorization_filter]]

        dependent_path ->
          [
            request_path ++ [:authorization_filter],
            [:side_load, Enum.reverse(Enum.map(dependent_path, &Map.get(&1, :name))), :data]
          ]
      end

    dependencies =
      if use_data_for_filter? do
        [[:data, :data] | dependencies]
      else
        dependencies
      end

    dependencies =
      if relationship.type == :many_to_many do
        join_relationship = join_relationship(relationship)

        [
          [
            :side_load,
            Enum.map(join_relationship_path(path, join_relationship), & &1.name),
            :data
          ]
          | dependencies
        ]
      else
        dependencies
      end

    source =
      [relationship | path]
      |> Enum.reverse()
      |> Enum.map_join(".", &Map.get(&1, :name))

    Engine.Request.new(
      action: Ash.Resource.primary_action!(relationship.destination, :read),
      resource: relationship.destination,
      name: "side_load #{source}",
      api: related_query.api,
      path: request_path,
      query:
        side_load_query(
          relationship,
          related_query,
          path,
          root_query,
          use_data_for_filter?
        ),
      data:
        Request.resolve(dependencies, fn data ->
          base_query =
            case get_in(data, request_path ++ [:authorization_filter]) do
              nil ->
                related_query

              authorization_filter ->
                Ash.Query.filter(related_query, authorization_filter)
            end

          with {:ok, new_query} <-
                 true_side_load_query(
                   relationship,
                   base_query,
                   data,
                   path,
                   root_data,
                   use_data_for_filter?
                 ),
               {:ok, results} <-
                 run_actual_query(
                   root_data,
                   new_query,
                   data,
                   path,
                   use_data_for_filter?,
                   relationship
                 ) do
            {:ok, results}
          else
            {:error, error} -> {:error, error}
          end
        end)
    )
  end

  defp join_relationship(relationship) do
    Ash.Resource.relationship(relationship.source, relationship.join_relationship)
  end

  defp join_relationship_path(path, join_relationship) do
    Enum.reverse([join_relationship | path])
  end

  defp join_assoc_request(
         relationship,
         related_query,
         root_query,
         path,
         root_data,
         use_data_for_filter?
       ) do
    join_relationship = join_relationship(relationship)
    join_relationship_name = join_relationship.name

    case path do
      [%{name: ^join_relationship_name} | _] ->
        nil

      path ->
        join_relationship_path = join_relationship_path(path, join_relationship)
        join_relationship_path_names = Enum.map(join_relationship_path, & &1.name)

        dependencies =
          if path == [] do
            [[:side_load, join_relationship_path_names, :authorization_filter]]
          else
            [
              [:side_load, join_relationship_path_names, :authorization_filter],
              [:side_load, Enum.reverse(Enum.map(path, &Map.get(&1, :name))), :data]
            ]
          end

        dependencies =
          if use_data_for_filter? do
            [[:data, :data] | dependencies]
          else
            dependencies
          end

        related_query = Ash.Query.new(join_relationship.destination, related_query.api)

        Request.new(
          action: Ash.Resource.primary_action!(relationship.destination, :read),
          resource: relationship.through,
          name: "side_load join #{join_relationship.name}",
          api: related_query.api,
          path: [:side_load, join_relationship_path_names],
          query:
            side_load_query(
              join_relationship,
              related_query,
              join_relationship_path,
              root_query,
              use_data_for_filter?
            ),
          data:
            Request.resolve(dependencies, fn data ->
              base_query =
                case get_in(data, [
                       :side_load,
                       join_relationship_path_names,
                       :authorization_filter
                     ]) do
                  nil ->
                    related_query

                  authorization_filter ->
                    Ash.Query.filter(related_query, authorization_filter)
                end

              with {:ok, new_query} <-
                     true_side_load_query(
                       join_relationship,
                       base_query,
                       data,
                       path,
                       root_data,
                       use_data_for_filter?
                     ),
                   {:ok, results} <-
                     run_actual_query(
                       root_data,
                       new_query,
                       data,
                       path,
                       use_data_for_filter?,
                       relationship
                     ) do
                {:ok, results}
              else
                {:error, error} -> {:error, error}
              end
            end)
        )
    end
  end

  defp run_actual_query(root_data, query, data, path, use_data?, relationship) do
    offset? = is_nil(query.offset) || query.offset != 0

    source_data =
      case path do
        [] when use_data? ->
          get_in(data, [:data, :data])

        [] ->
          root_data

        path ->
          data
          |> Map.get(:side_load, %{})
          |> Map.get(path, %{})
          |> Map.get(:data, [])
      end

    cond do
      (query.limit ||
         offset?) && relationship.type != :many_to_many &&
          Ash.Resource.data_layer_can?(
            relationship.source,
            {:lateral_join, relationship.destination}
          ) ->
        query
        |> Ash.Query.set_context(%{
          data_layer: %{
            lateral_join_source:
              {source_data, relationship.source, relationship.source_field,
               relationship.destination_field}
          }
        })
        |> query.api.read()

      (query.limit || offset?) && relationship.type != :many_to_many ->
        artificial_limit_and_offset(query, relationship)

      true ->
        query.api.read(query)
    end
  end

  defp artificial_limit_and_offset(query, relationship) do
    case query.api.read(query) do
      {:ok, results} ->
        new_results =
          results
          |> Enum.group_by(fn record ->
            Map.get(record, relationship.destination_key)
          end)
          |> Enum.flat_map(fn {_, group} ->
            offset_records = Enum.drop(group, query.offset || 0)

            if query.limit do
              Enum.take(offset_records, query.limit)
            else
              offset_records
            end
          end)

        {:ok, new_results}

      {:error, error} ->
        {:error, error}
    end
  end

  defp side_load_query_with_reverse_path(
         root_query,
         related_query,
         reverse_path,
         root_data_filter
       ) do
    case Ash.Filter.parse(root_query.resource, root_query.filter) do
      {:ok, parsed} ->
        related_query
        |> Ash.Query.unset(:side_load)
        |> Ash.Query.filter(
          put_nested_relationship(
            [],
            reverse_path,
            root_data_filter,
            false
          )
        )
        |> Ash.Query.filter(put_nested_relationship([], reverse_path, parsed, false))
        |> Ash.Query.filter(related_query.filter)
        |> extract_errors()

      {:error, error} ->
        {:error, error}
    end
  end

  defp side_load_query(
         relationship,
         related_query,
         path,
         root_query,
         true
       ) do
    Request.resolve([[:data, :data]], fn %{data: %{data: data}} ->
      root_data_filter =
        case data do
          [] ->
            false

          [%resource{} = item] ->
            item
            |> Map.take(Ash.Resource.primary_key(resource))
            |> Enum.to_list()

          [%resource{} | _] = items ->
            pkey = Ash.Resource.primary_key(resource)
            [or: Enum.map(items, fn item -> item |> Map.take(pkey) |> Enum.to_list() end)]
        end

      case reverse_relationship_path(relationship, Enum.drop(path, 1)) do
        {:ok, reverse_path} ->
          side_load_query_with_reverse_path(
            root_query,
            related_query,
            reverse_path,
            root_data_filter
          )

        _ ->
          relationship.destination
          |> Ash.Query.new(related_query.api)
          |> Ash.Query.filter(related_query.filter)
          |> extract_errors()
      end
    end)
  end

  defp side_load_query(
         relationship,
         related_query,
         path,
         root_query,
         false
       ) do
    case reverse_relationship_path(relationship, path) do
      {:ok, reverse_path} ->
        case Ash.Filter.parse(root_query.resource, root_query.filter) do
          {:ok, parsed} ->
            related_query
            |> Ash.Query.filter(
              put_nested_relationship(
                [],
                reverse_path,
                parsed,
                false
              )
            )
            |> Ash.Query.unset(:side_load)

          {:error, error} ->
            {:error, error}
        end

      :error ->
        related_query
        |> Ash.Query.unset(:side_load)
    end
  end

  defp extract_errors(%{errors: []} = item), do: {:ok, item}
  defp extract_errors(%{errors: errors}), do: {:error, errors}

  defp true_side_load_query(relationship, query, data, path, root_data, use_data?) do
    {source_field, path} =
      if relationship.type == :many_to_many do
        join_relationship = join_relationship(relationship)

        {relationship.destination_field_on_join_table,
         join_relationship_path(path, join_relationship) |> Enum.map(& &1.name)}
      else
        {relationship.source_field, path |> Enum.reverse() |> Enum.map(& &1.name)}
      end

    source_data =
      case path do
        [] when use_data? ->
          Map.get(data, :data)

        [] ->
          %{data: root_data}

        path ->
          data
          |> Map.get(:side_load, %{})
          |> Map.get(path, %{})
      end

    get_query(query, relationship, source_data, source_field)
  end

  defp get_query(query, relationship, source_data, source_field) do
    offset? = is_nil(query.offset) || query.offset != 0

    cond do
      (query.limit ||
         offset?) && relationship.type != :many_to_many &&
          Ash.Resource.data_layer_can?(
            relationship.source,
            {:lateral_join, relationship.destination}
          ) ->
        {:ok, Ash.Query.unset(query, :side_load)}

      (query.limit || offset?) && relationship.type != :many_to_many ->
        {:ok, Ash.Query.unset(query, [:side_load, :limit, :offset])}

      true ->
        related_data = Map.get(source_data || %{}, :data, [])

        ids =
          Enum.flat_map(related_data, fn data ->
            data
            |> Map.get(source_field)
            |> List.wrap()
          end)

        filter_value =
          case ids do
            [id] ->
              id

            ids ->
              [in: ids]
          end

        new_query =
          query
          |> Ash.Query.filter([{relationship.destination_field, filter_value}])

        {:ok, new_query}
    end
  end

  def reverse_relationship_path(relationship, prior_path, acc \\ [])

  def reverse_relationship_path(relationship, [], acc) do
    relationship.destination
    |> Ash.Resource.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        {:ok, Enum.reverse([reverse.name | acc])}
    end
  end

  def reverse_relationship_path(relationship, [next_relationship | rest], acc) do
    relationship.destination
    |> Ash.Resource.relationships()
    |> Enum.find(fn destination_relationship ->
      reverse_relationship?(relationship, destination_relationship)
    end)
    |> case do
      nil ->
        :error

      reverse ->
        reverse_relationship_path(next_relationship, rest, [reverse.name | acc])
    end
  end

  defp reverse_relationship?(rel, destination_rel) do
    rel.source == destination_rel.destination &&
      rel.source_field == destination_rel.destination_field &&
      rel.destination_field == destination_rel.source_field &&
      Map.fetch(rel, :source_field_on_join_table) ==
        Map.fetch(destination_rel, :destination_field_on_join_table) &&
      Map.fetch(rel, :destination_field_on_join_table) ==
        Map.fetch(destination_rel, :source_field_on_join_table)
  end

  defp put_nested_relationship(request_filter, path, value, records?) when not is_list(value) do
    put_nested_relationship(request_filter, path, [value], records?)
  end

  defp put_nested_relationship(request_filter, [rel | rest], values, records?) do
    [
      {rel, put_nested_relationship(request_filter, rest, values, records?)}
    ]
  end

  defp put_nested_relationship(request_filter, [], [{field, value}], _) do
    [{field, value} | request_filter]
  end

  defp put_nested_relationship(request_filter, [], [{field, _} | _] = keys, _) do
    [{field, [{:in, Enum.map(keys, &elem(&1, 1))}]} | request_filter]
  end

  defp put_nested_relationship(request_filter, [], [values], _) do
    List.wrap(request_filter) ++ List.wrap(values)
  end

  defp put_nested_relationship(request_filter, [], values, _) do
    Keyword.update(request_filter, :or, values, &Kernel.++(&1, values))
  end
end
