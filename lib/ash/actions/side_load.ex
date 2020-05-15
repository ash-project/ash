defmodule Ash.Actions.SideLoad do
  def requests(query, use_data_for_filter? \\ true, root_query \\ nil, path \\ [])
  def requests(nil, _, _, _), do: {:ok, []}
  def requests(%{side_load: []}, _, _root_query, _), do: {:ok, []}

  def requests(%{side_load: side_loads} = query, use_data_for_filter?, root_query, path) do
    root_query = root_query || query

    requests =
      side_loads
      |> List.wrap()
      |> Enum.flat_map(fn {relationship, further} ->
        relationship = Ash.relationship(query.resource, relationship)

        related_query =
          case further do
            %Ash.Query{} = query ->
              query

            further ->
              root_query.api
              |> Ash.Query.new(relationship.destination)
              |> Ash.Query.side_load(further)
          end

        new_path = [relationship | path]

        case requests(related_query, use_data_for_filter?, root_query, new_path) do
          {:ok, further_requests} ->
            further_requests ++
              do_requests(relationship, related_query, new_path, root_query, use_data_for_filter?)

          {:error, error} ->
            # TODO: This case isn't actually reachable because the only errors that can occur
            # currently happen as the requests are resolved. Either 1.) update this interface
            # to just return a list or 2.) surface those errors sooner
            {:error, error}
        end
      end)

    {:ok, requests}
  end

  def side_load(data, query, opts \\ [])
  def side_load([], _query, _opts), do: {:ok, []}
  def side_load(nil, _query, _opts), do: {:ok, nil}

  def side_load(data, %{side_load: []}, _opts), do: {:ok, data}

  def side_load(data, query, opts) when not is_list(data) do
    data
    |> List.wrap()
    |> side_load(query, opts)
    |> case do
      {:ok, [record]} -> {:ok, record}
      {:error, error} -> {:error, error}
    end
  end

  def side_load([%resource{} | _] = data, side_load_query, opts) do
    api = side_load_query.api

    {:ok, pkey_filters} =
      Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(resource, data)

    new_query = Ash.Query.filter(side_load_query, or: pkey_filters)

    case requests(new_query, false) do
      {:ok, requests} ->
        result =
          if opts[:authorization] do
            Ash.Engine.run(
              requests,
              api,
              user: opts[:authorization][:user],
              bypass_strict_access?: opts[:authorization][:bypass_strict_access?],
              verbose?: opts[:verbose?]
            )
          else
            Ash.Engine.run(requests, api, fetch_only?: true, verbose?: opts[:verbose?])
          end

        case result do
          %{data: %{include: _} = state, errors: errors} when errors == [] ->
            {:ok, attach_side_loads(data, state)}

          %{errors: errors} ->
            {:error, errors}
        end

      {:error, errors} ->
        {:error, errors}
    end
  end

  def attach_side_loads([%resource{} | _] = data, %{include: includes})
      when is_list(data) do
    includes
    |> Enum.sort_by(fn {key, _value} ->
      last_relationship = last_relationship!(resource, key)
      # We want to do `many_to_many` last, so we know we've
      # done their join assocs first. Pretty hacky
      {length(key), last_relationship.type == :many_to_many}
    end)
    |> Enum.reduce(data, fn {key, %{data: value}}, data ->
      last_relationship = last_relationship!(resource, key)
      lead_path = :lists.droplast(key)

      case last_relationship do
        %{type: :many_to_many, name: name} ->
          # TODO: If we sort the relationships as we do them (doing the join assoc first)
          # then we can just use those linked assocs (maybe)
          join_association = String.to_existing_atom(to_string(name) <> "_join_assoc")

          join_path = lead_path ++ [join_association]

          join_data =
            includes
            |> Map.get(join_path, %{})
            |> Map.get(:data, [])

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

        %{cardinality: :many} ->
          values = Enum.group_by(value, &Map.get(&1, last_relationship.destination_field))

          map_or_update(data, lead_path, fn record ->
            source_key = Map.get(record, last_relationship.source_field)
            related_records = Map.get(values, source_key, [])
            Map.put(record, last_relationship.name, related_records)
          end)

        %{cardinality: :one} ->
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

  defp map_or_update(record, [], func) when not is_list(record), do: func.(record)

  defp map_or_update(records, [], func) do
    Enum.map(records, func)
  end

  defp map_or_update(records, [path | tail], func) do
    map_or_update(records, [], fn record ->
      Map.update!(record, path, &map_or_update(&1, tail, func))
    end)
  end

  defp last_relationship!(resource, [last]) do
    Ash.relationship(resource, last) || raise "Assumption Failed"
  end

  defp last_relationship!(resource, [first | rest]) do
    relationship = Ash.relationship(resource, first) || raise "Assumption Failed"

    last_relationship!(relationship.destination, rest)
  end

  defp do_requests(relationship, related_query, path, root_query, use_data_for_filter?) do
    side_load_request =
      side_load_request(relationship, related_query, root_query, path, use_data_for_filter?)

    case relationship.type do
      :many_to_many ->
        case join_assoc_request(
               relationship,
               related_query,
               root_query,
               path,
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

  defp side_load_request(relationship, related_query, root_query, path, use_data_for_filter?) do
    default_read =
      Ash.primary_action(relationship.destination, :read) ||
        raise "Must set default read for #{inspect(relationship.destination)}"

    dependencies =
      case path do
        [] ->
          []

        dependent_path ->
          [[:include, Enum.reverse(Enum.map(dependent_path, &Map.get(&1, :name))), :data]]
      end

    request_path = [:include, Enum.reverse(Enum.map(path, &Map.get(&1, :name)))]

    dependencies =
      if use_data_for_filter? do
        [[:data, :data] | dependencies]
      else
        [request_path ++ [:query] | dependencies]
      end

    dependencies =
      if relationship.type == :many_to_many do
        join_relationship = join_relationship(relationship)
        [[:include, join_relationship_path(path, join_relationship), :data] | dependencies]
      else
        dependencies
      end

    source =
      [relationship | path]
      |> Enum.reverse()
      |> Enum.map_join(".", &Map.get(&1, :name))

    Ash.Engine.Request.new(
      action_type: :read,
      resource: relationship.destination,
      rules: default_read.rules,
      name: "side_load #{source}",
      api: related_query.api,
      path: request_path,
      resolve_when_fetch_only?: true,
      query:
        side_load_query(
          relationship,
          related_query,
          path,
          root_query,
          use_data_for_filter?
        ),
      strict_access?: true,
      data:
        Ash.Engine.Request.resolve(dependencies, fn data ->
          # Because we have the records, we can optimize the filter by nillifying the reverse relationship,
          # and regenerating.
          # The reverse relationship is useful if you don't have the relationship keys for the related items (only pkeys)
          # or for doing many to many joins, but can be slower.
          # If the relationship is already loaded, we should consider doing an in-memory filtering
          # Right now, we just use the original query

          new_query =
            if use_data_for_filter? do
              true_side_load_query(
                relationship,
                related_query,
                data,
                path
              )
            else
              {:ok,
               Enum.reduce(request_path ++ [:query], data, fn path_part, data ->
                 Map.get(data, path_part)
               end)}
            end

          with {:ok, query} <- new_query,
               {:ok, results} <- related_query.api.read(query) do
            {:ok, results}
          else
            {:error, error} -> {:error, error}
          end
        end)
    )
  end

  defp join_relationship(relationship) do
    Ash.relationship(
      relationship.source,
      String.to_existing_atom(to_string(relationship.name) <> "_join_assoc")
    )
  end

  defp join_relationship_path(path, join_relationship) do
    Enum.reverse(Enum.map([join_relationship | path], & &1.name))
  end

  defp join_assoc_request(relationship, related_query, root_query, path, use_data_for_filter?) do
    join_relationship = join_relationship(relationship)
    join_relationship_name = join_relationship.name

    case path do
      [%{name: ^join_relationship_name} | _] ->
        nil

      path ->
        join_relationship_path = join_relationship_path(path, join_relationship)

        default_read =
          Ash.primary_action(join_relationship.source, :read) ||
            raise "Must set default read for #{inspect(relationship.destination)}"

        dependencies =
          cond do
            path == [] ->
              []

            true ->
              [[:include, Enum.map(path, &Map.get(&1, :name)), :data]]
          end

        dependencies =
          if use_data_for_filter? do
            [[:data, :data] | dependencies]
          else
            [[:include, join_relationship_path, :query] | dependencies]
          end

        related_query = related_query.api.query(join_relationship.destination)

        Ash.Engine.Request.new(
          action_type: :read,
          resource: relationship.through,
          rules: default_read.rules,
          name: "side_load join #{join_relationship.name}",
          api: related_query.api,
          path: [:include, join_relationship_path],
          strict_access?: true,
          resolve_when_fetch_only?: true,
          query:
            side_load_query(
              join_relationship,
              related_query,
              join_relationship_path,
              root_query,
              use_data_for_filter?
            ),
          strict_access?: true,
          data:
            Ash.Engine.Request.resolve(dependencies, fn data ->
              new_query =
                if use_data_for_filter? do
                  true_side_load_query(
                    join_relationship,
                    related_query,
                    data,
                    path
                  )
                else
                  {:ok,
                   Enum.reduce([:include, join_relationship_path] ++ [:query], data, fn path_part,
                                                                                        data ->
                     Map.get(data, path_part)
                   end)}
                end

              with {:ok, query} <- new_query,
                   {:ok, results} <- related_query.api.read(query) do
                {:ok, results}
              else
                {:error, error} -> {:error, error}
              end
            end)
        )
    end
  end

  defp side_load_query(
         %{reverse_relationship: nil, type: :many_to_many} = relationship,
         _related_query,
         _path,
         _root_query,
         _use_data_for_filter?
       ) do
    Ash.Engine.Request.resolve(fn _ ->
      {:error,
       "Required reverse relationship for #{inspect(relationship)} TODO: this can be resolved"}
    end)
  end

  defp side_load_query(
         relationship,
         related_query,
         path,
         _root_query,
         true
       ) do
    Ash.Engine.Request.resolve([[:data, :data]], fn %{data: %{data: data}} ->
      root_filter =
        case data do
          [] ->
            [__impossible__: true]

          [%resource{} = item] ->
            item
            |> Map.take(Ash.primary_key(resource))
            |> Enum.to_list()

          [%resource{} | _] = items ->
            pkey = Ash.primary_key(resource)
            [or: Enum.map(items, fn item -> item |> Map.take(pkey) |> Enum.to_list() end)]
        end

      case reverse_relationship_path(relationship, path) do
        {:ok, reverse_path} ->
          related_query
          |> Ash.Query.filter(
            put_nested_relationship(root_filter, reverse_path, root_filter, false)
          )
          |> extract_errors()

        _ ->
          relationship.destination
          |> Ash.Filter.parse(
            related_query.filter,
            related_query.api
          )
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
        Ash.Query.filter(
          related_query,
          put_nested_relationship([], reverse_path, root_query.filter, false)
        )

      :error ->
        related_query
    end
  end

  defp extract_errors(%{errors: []} = item), do: {:ok, item}
  defp extract_errors(%{errors: errors}), do: {:error, errors}

  defp true_side_load_query(
         %{type: :many_to_many, reverse_relationship: nil} = relationship,
         _filter,
         _data,
         _path
       ) do
    {:error, "Required reverse relationship for #{inspect(relationship)}"}
  end

  defp true_side_load_query(
         %{reverse_relationship: reverse_relationship, source: source} = relationship,
         query,
         data,
         path
       ) do
    pkey = Ash.primary_key(source)

    source_data =
      case path do
        [] ->
          Map.get(data, :data)

        path ->
          path_names = path |> Enum.reverse() |> Enum.map(& &1.name)

          data
          |> Map.get(:include, %{})
          |> Map.get(path_names, %{})
      end

    related_data = Map.get(source_data || %{}, :data, [])

    cond do
      reverse_relationship ->
        values = get_fields(related_data, pkey)

        new_query =
          Ash.Query.filter(query, put_nested_relationship([], [reverse_relationship], values))

        {:ok, new_query}

      true ->
        ids = Enum.map(related_data, &Map.get(&1, relationship.source_field))

        filter_value =
          case ids do
            [id] ->
              id

            ids ->
              [in: ids]
          end

        new_query = Ash.Query.filter(query, [{relationship.destination_field, filter_value}])

        {:ok, new_query}
    end
  end

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

  defp put_nested_relationship(request_filter, path, value, records? \\ true)
  # TODO: The assumption this is making really needs to be checked!
  defp put_nested_relationship(_, _, [], true), do: [__impossible__: true]
  defp put_nested_relationship(_, _, nil, true), do: [__impossible__: true]
  defp put_nested_relationship(_, _, [], false), do: []
  defp put_nested_relationship(_, _, nil, false), do: []

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
    request_filter ++ values
  end

  defp put_nested_relationship(request_filter, [], values, _) do
    Keyword.update(request_filter, :or, values, &Kernel.++(&1, values))
  end
end
