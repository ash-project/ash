defmodule Ash.Actions.SideLoad do
  def requests(
        api,
        resource,
        side_load,
        side_load_filters,
        root_filter,
        path \\ [],
        seed_data \\ nil
      )

  def requests(_, _, [], _, _, _, _), do: {:ok, []}

  def requests(api, resource, side_load, side_load_filters, root_filter, path, seed_data) do
    Enum.reduce(side_load, {:ok, []}, fn
      _, {:error, error} ->
        {:error, error}

      {key, true}, {:ok, acc} ->
        do_requests(api, resource, side_load_filters, key, [], root_filter, path, acc, seed_data)

      {key, further}, {:ok, acc} ->
        do_requests(
          api,
          resource,
          side_load_filters,
          key,
          further,
          root_filter,
          path,
          acc,
          seed_data
        )

      key, {:ok, acc} ->
        do_requests(api, resource, side_load_filters, key, [], root_filter, path, acc, seed_data)
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

  def side_load(api, resource, data, side_load, root_filter, side_load_filters \\ %{}) do
    case requests(api, resource, side_load, side_load_filters, root_filter, [], data) do
      {:ok, [_req | _] = requests} ->
        case Ash.Engine.run(requests, api) do
          %{data: %{data: %{data: data} = state}, errors: errors} when errors == %{} ->
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

          %{errors: errors} ->
            {:error, errors}
        end

      {:ok, []} ->
        {:ok, data}

      {:error, error} ->
        {:error, error}
    end
  end

  def attach_side_loads(%Ash.Actions.Paginator{results: results} = paginator, state) do
    %{paginator | results: attach_side_loads(results, state)}
  end

  def attach_side_loads([%resource{} | _] = data, %{include: includes})
      when is_list(data) do
    includes
    |> Enum.sort_by(fn {key, _value} ->
      length(key)
    end)
    |> Enum.reduce(data, fn {key, %{data: value}}, data ->
      last_relationship = last_relationship!(resource, key)

      case last_relationship do
        %{type: :many_to_many, name: name} ->
          # TODO: If we sort the relationships as we do them (doing the join assoc first)
          # then we can just use those linked assocs (maybe)
          join_association = String.to_existing_atom(to_string(name) <> "_join_assoc")
          join_path = :lists.droplast(key) ++ [join_association]
          join_data = Map.get(includes, join_path, [])

          map_or_update(data, fn record ->
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

        %{cardinality: :many} ->
          values = Enum.group_by(value, &Map.get(&1, last_relationship.destination_field))

          map_or_update(data, fn record ->
            source_key = Map.get(record, last_relationship.source_field)
            related_records = Map.get(values, source_key, [])
            Map.put(record, last_relationship.name, related_records)
          end)

        %{cardinality: :one} ->
          values =
            Enum.into(value, %{}, fn item ->
              {Map.get(item, last_relationship.destination_field), item}
            end)

          map_or_update(data, fn record ->
            source_key = Map.get(record, last_relationship.source_field)
            related_record = Map.get(values, source_key)
            Map.put(record, last_relationship.name, related_record)
          end)

          {values, nil}
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

  defp map_or_update(record, func) when not is_list(record), do: func.(record)

  defp map_or_update(records, func) do
    Enum.map(records, func)
  end

  defp last_relationship!(resource, [last]) do
    Ash.relationship(resource, last) || raise "Assumption Failed"
  end

  defp last_relationship!(resource, [first | rest]) do
    relationship = Ash.relationship(resource, first) || raise "Assumption Failed"

    last_relationship!(relationship.destination, rest)
  end

  defp do_requests(api, resource, filters, key, further, root_filter, path, acc, seed_data) do
    with {:rel, relationship} when not is_nil(relationship) <-
           {:rel, Ash.relationship(resource, key)},
         nested_path <- path ++ [relationship],
         {:ok, requests} <-
           requests(api, relationship.destination, further, filters, nested_path) do
      default_read =
        Ash.primary_action(relationship.destination, :read) ||
          raise "Must set default read for #{inspect(resource)}"

      data_dependency =
        if seed_data do
          [[:data]]
        else
          []
        end

      dependencies =
        if path == [] do
          data_dependency
        else
          data_dependency ++ [[:include, Enum.map(path, &Map.get(&1, :name)), :data]]
        end

      source =
        nested_path
        |> Enum.reverse()
        |> Enum.map_join(".", &Map.get(&1, :name))

      request =
        Ash.Engine.Request.new(
          action_type: :read,
          resource: relationship.destination,
          rules: default_read.rules,
          name: "side_load #{source}",
          api: api,
          path: [:include, Enum.map(nested_path, &Map.get(&1, :name))],
          resolve_when_fetch_only?: true,
          filter:
            side_load_filter2(
              relationship,
              Map.get(filters || %{}, source, []),
              nested_path,
              root_filter,
              data_dependency,
              seed_data
            ),
          strict_access?: true,
          data:
            Ash.Engine.Request.UnresolvedField.data(dependencies, fn data ->
              data =
                if seed_data do
                  Map.update(data, :data, %{data: seed_data}, fn data_request ->
                    Map.put(data_request, :data, seed_data)
                  end)
                else
                  data
                end

              # Because we have the records, we can optimize the filter by nillifying the reverse relationship,
              # and regenerating.
              # The reverse relationship is useful if you don't have the relationship keys for the related items (only pkeys)
              # or for doing many to many joins, but can be slower.
              # If the relationship is already loaded, we should consider doing an in-memory filtering
              # Right now, we just use the original query
              with {:ok, filter} <-
                     true_side_load_filter(
                       relationship,
                       Map.get(filters || %{}, source, []),
                       data,
                       path
                     ),
                   {:ok, %{results: results}} <-
                     api.read(relationship.destination, filter: filter, paginate: false) do
                {:ok, results}
              else
                {:error, error} -> {:error, error}
              end
            end)
        )

      requests_with_sideload =
        if relationship.type == :many_to_many do
          join_relationship =
            Ash.relationship(
              resource,
              String.to_existing_atom(to_string(relationship.name) <> "_join_assoc")
            )

          join_relationship_path =
            Enum.reverse(Enum.map(path, & &1.name)) ++ [join_relationship.name]

          side_load_request =
            Ash.Engine.Request.new(
              action_type: :read,
              resource: relationship.through,
              rules: default_read.rules,
              name: "side_load join #{join_relationship.name}",
              api: api,
              path: [:include, join_relationship_path],
              strict_access?: root_filter not in [:create, :update],
              resolve_when_fetch_only?: true,
              filter:
                side_load_filter2(
                  Ash.relationship(resource, join_relationship.name),
                  [],
                  nested_path,
                  root_filter,
                  data_dependency,
                  seed_data
                ),
              strict_access?: true,
              data:
                Ash.Engine.Request.UnresolvedField.data(dependencies, fn data ->
                  if seed_data do
                    Map.update(data, :data, %{data: seed_data}, fn data_request ->
                      Map.put(data_request, :data, seed_data)
                    end)
                  else
                    data
                  end

                  with {:ok, filter} <-
                         true_side_load_filter(
                           join_relationship,
                           [],
                           data,
                           path
                         ),
                       {:ok, %{results: results}} <-
                         api.read(join_relationship.destination,
                           filter: filter,
                           paginate: false
                         ) do
                    {:ok, results}
                  else
                    {:error, error} -> {:error, error}
                  end
                end)
            )

          [side_load_request | requests]
        else
          requests
        end

      {:ok, [request | requests_with_sideload] ++ acc}
    else
      {:rel, nil} -> {:error, "no such relationship: #{key} on: #{resource}"}
      {:error, error} -> {:error, error}
    end
  end

  defp side_load_filter2(
         %{reverse_relationship: nil, type: :many_to_many} = relationship,
         _request_filter,
         _prior_path,
         _root_filter,
         _,
         _
       ) do
    Ash.Engine.Request.UnresolvedField.field([], fn _ ->
      {:error, "Required reverse relationship for #{inspect(relationship)}"}
    end)
  end

  defp side_load_filter2(
         relationship,
         request_filter,
         prior_path,
         root_filter,
         data_dependency,
         seed_data
       )
       when root_filter in [:update, :create] do
    Ash.Engine.Request.UnresolvedField.field(data_dependency, fn data ->
      data =
        if seed_data do
          seed_data
        else
          # I'm a failure
          data.data.data
        end

      root_filter =
        case data do
          [%resource{} = item] ->
            item
            |> Map.take(Ash.primary_key(resource))
            |> Enum.to_list()

          [%resource{} | _] = items ->
            pkey = Ash.primary_key(resource)
            [or: Enum.map(items, fn item -> item |> Map.take(pkey) |> Enum.to_list() end)]
        end

      case reverse_relationship_path(relationship, prior_path) do
        {:ok, reverse_path} ->
          Ash.Filter.parse(
            relationship.destination,
            put_nested_relationship(request_filter, reverse_path, root_filter)
          )
      end
    end)
  end

  defp side_load_filter2(
         relationship,
         request_filter,
         prior_path,
         root_filter,
         _data_dependency,
         _seed_data
       ) do
    # TODO: If the root request is non `strict_access?`, then we could actually
    # do something like this, using the full path. For now, we'll just authorize
    # with the filter that is provided, since adding id filters to that
    # (if reverse relationship is nil)
    # Ash.Engine.Request.UnresolvedField.field(dependencies, fn
    #   %{path: [:include, [_]]}, _, %{data: data} ->
    #     new_values = Enum.map(data, &Map.get(&1, relationship.source_field))

    #     filter =
    #       add_relationship_id_filter(request_filter, relationship.destination_field, new_values)

    #     Ash.Filter.parse(relationship.destination, filter, api)

    #   %{path: [:include, include_path]}, _, data ->
    #     prior_path = include_path |> Enum.reverse() |> tl()
    #     source_data = Map.get(data, [:include, prior_path])
    #     new_values = Enum.map(source_data, &Map.get(&1, relationship.source_field))

    #     filter =
    #       add_relationship_id_filter(request_filter, relationship.destination_field, new_values)

    #     Ash.Filter.parse(relationship.destination, filter, api)
    # end)
    # prior_path = include_path |> Enum.reverse() |> tl()
    # TODO: clean up return of this function
    # {:ok, path, values} = reverse_relationship_path_and_values(relationship, data, prior_path)
    case reverse_relationship_path(relationship, prior_path) do
      {:ok, reverse_path} ->
        Ash.Filter.parse(
          relationship.destination,
          put_nested_relationship(request_filter, reverse_path, root_filter)
        )

      :error ->
        Ash.Filter.parse(
          relationship.destination,
          request_filter
        )
    end
  end

  defp true_side_load_filter(
         %{type: :many_to_many, reverse_relationship: nil} = relationship,
         _filter,
         _data,
         _path
       ) do
    {:error, "Required reverse relationship for #{inspect(relationship)}"}
  end

  defp true_side_load_filter(
         %{reverse_relationship: reverse_relationship, source: source} = relationship,
         filter,
         data,
         path
       ) do
    pkey = Ash.primary_key(source)

    source_data =
      case path do
        [] ->
          Map.get(data, :data)

        path ->
          Map.get(data, [:include, Enum.reverse(path)])
      end

    values = get_fields(source_data.data, pkey)

    cond do
      reverse_relationship ->
        {:ok, put_nested_relationship(filter, [reverse_relationship], values)}

      true ->
        ids = Enum.map(source_data.data, &Map.get(&1, relationship.source_field))

        filter_value =
          case ids do
            [id] ->
              id

            ids ->
              [in: ids]
          end

        new_filter =
          if Keyword.has_key?(filter, relationship.destination_field) do
            clause = [{relationship.destination_field, filter_value}]

            Keyword.update(filter, :and, [clause], fn ands ->
              [clause | ands]
            end)
          else
            Keyword.put(filter, relationship.destination_field, filter_value)
          end

        {:ok, new_filter}
    end
  end

  defp add_relationship_id_filter(request_filter, field, [filter_value]) do
    case Keyword.fetch(request_filter, field) do
      {:ok, value} when value == filter_value ->
        request_filter

      {:ok, value} ->
        request_filter
        |> Keyword.put_new(:and, [])
        |> Keyword.update!(:and, fn ands ->
          ands ++ [[{field, value}], [{field, filter_value}]]
        end)

      _ ->
        Keyword.put(request_filter, field, filter_value)
    end
  end

  defp add_relationship_id_filter(request_filter, field, new_values) do
    Keyword.update(request_filter, field, [in: new_values], fn field_filter ->
      Keyword.update(field_filter, :in, new_values, &Kernel.++(&1, new_values))
    end)
  end

  # defp reverse_relationship_path_and_values(relationship, data, prior_path, acc \\ [])

  # defp reverse_relationship_path_and_values(%{reverse_relationship: nil}, _, _, _) do
  #   :error
  # end

  # defp reverse_relationship_path_and_values(relationship, data, [], acc) do
  #   path = Enum.reverse([relationship.reverse_relationship | acc])
  #   pkey = Ash.primary_key(relationship.source)

  #   values = get_fields(data, pkey)

  #   {:ok, path, values}
  # end

  # defp reverse_relationship_path_and_values(relationship, data, [next_relationship | rest], acc) do
  #   reverse_relationship_path_and_values(next_relationship, data, rest, [
  #     relationship.reverse_relationship | acc
  #   ])
  # end

  defp reverse_relationship_path(relationship, prior_path, acc \\ [])

  defp reverse_relationship_path(relationship, [], acc) do
    case relationship.reverse_relationship do
      nil ->
        :error

      reverse ->
        {:ok, Enum.reverse([reverse | acc])}
    end
  end

  defp reverse_relationship_path(relationship, [next_relationship | rest], acc) do
    case relationship.reverse_relationship do
      nil ->
        :error

      reverse ->
        reverse_relationship_path(next_relationship, rest, [reverse | acc])
    end
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

  # defp get_field(%{data: data}, name, []) do
  #   data
  #   |> Enum.map(&Map.get(&1, name))
  #   |> Enum.uniq()
  # end

  # defp get_field(%{data: data}, name, [first | rest]) do
  #   data
  #   |> Enum.flat_map(fn item ->
  #     item
  #     |> Map.get(first)
  #     |> List.wrap()
  #   end)
  #   |> get_field(name, rest)
  # end

  defp put_nested_relationship(_, _, []), do: [__impossible__: true]
  defp put_nested_relationship(_, _, nil), do: [__impossible__: true]

  defp put_nested_relationship(request_filter, path, value) when not is_list(value) do
    put_nested_relationship(request_filter, path, [value])
  end

  defp put_nested_relationship(request_filter, [rel | rest], values) do
    [
      {rel, put_nested_relationship(request_filter, rest, values)}
    ]
  end

  defp put_nested_relationship(request_filter, [], [[{field, _}] | _] = keys) do
    add_relationship_id_filter(request_filter, field, Enum.map(keys, &elem(&1, 1)))
  end

  defp put_nested_relationship(request_filter, [], [values]) do
    Enum.reduce(values, request_filter, fn {field, value}, filter ->
      add_relationship_id_filter(filter, field, [value])
    end)
  end

  defp put_nested_relationship(request_filter, [], values) do
    Keyword.update(request_filter, :or, values, &Kernel.++(&1, values))
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
