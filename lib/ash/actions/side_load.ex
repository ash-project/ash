defmodule Ash.Actions.SideLoad do
  def requests(api, resource, side_load, source_filter, path \\ [])
  def requests(_, _, [], _, _), do: {:ok, []}

  def requests(api, resource, side_load, source_filter, path) do
    # TODO: return authorizations here.
    Enum.reduce(side_load, {:ok, []}, fn
      _, {:error, error} ->
        {:error, error}

      {key, true}, {:ok, acc} ->
        do_requests(api, resource, key, [], source_filter, path, acc)

      {key, further}, {:ok, acc} ->
        do_requests(api, resource, key, further, source_filter, path, acc)

      key, {:ok, acc} ->
        do_requests(api, resource, key, [], source_filter, path, acc)
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

  def side_load(api, resource, data, side_load, source_filter) do
    requests = requests(api, resource, side_load, source_filter)

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

  defp do_requests(api, resource, key, further, source_filter, path, acc) do
    with {:rel, relationship} when not is_nil(relationship) <-
           {:rel, Ash.relationship(resource, key)},
         nested_path <- path ++ [relationship],
         {:ok, requests} <-
           requests(api, relationship.destination, further, source_filter, nested_path) do
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
                # or for doing many to many joins.
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

  # defp add_results_to_data(data, results, relationship, path) do
  #   {results_by_destination_key, default} =
  #     case relationship.cardinality do
  #       :many ->
  #         {Enum.group_by(results, &Map.get(&1, relationship.destination_field)), []}

  #       :one ->
  #         keyed =
  #           Enum.into(results, %{}, fn result ->
  #             {Map.get(result, relationship.destination_field), result}
  #           end)

  #         {keyed, nil}
  #     end

  #   do_add_results_to_data(data, results_by_destination_key, default, relationship, path)
  # end

  # defp do_add_results_to_data(data, results_by_destination_key, default, relationship, [])
  #      when is_list(data) do
  #   Enum.map(data, fn item ->
  #     source_value = Map.get(item, relationship.source_field)
  #     relationship_value = Map.get(results_by_destination_key, source_value, default)

  #     Map.put(item, relationship.name, relationship_value)
  #   end)
  # end

  # defp do_add_results_to_data(data, results_by_destination_key, default, relationship, [
  #        first | rest
  #      ])
  #      when is_list(data) do
  #   Enum.map(data, fn item ->
  #     Map.update!(
  #       item,
  #       first,
  #       &do_add_results_to_data(&1, results_by_destination_key, default, relationship, rest)
  #     )
  #   end)
  # end

  # defp do_add_results_to_data(data, results_by_destination_key, default, relationship, path) do
  #   [data]
  #   |> do_add_results_to_data(results_by_destination_key, default, relationship, path)
  #   |> List.first()
  # end

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

  defp reverse_relationship_path_and_values(
         %{reverse_relationship: nil},
         _data,
         _prior_path,
         _acc
       ) do
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

  # defp is_fetched?(_, []), do: true

  # defp is_fetched?(records, [rel | rest]) do
  #   Enum.all?(records, fn record ->
  #     case Map.get(record, rel) do
  #       %Ecto.Association.NotLoaded{} ->
  #         false

  #       value ->
  #         value
  #         |> List.wrap()
  #         |> is_fetched?(rest)
  #     end
  #   end)
  # end

  defp put_nested_relationship([rel | rest], value) do
    [
      {rel, put_nested_relationship(rest, value)}
    ]
  end

  defp put_nested_relationship([], value) do
    value
  end

  # def side_load(resource, record, keyword, api, global_params \\ [])

  # def side_load(_resource, [], _side_loads, _api, _global_params) do
  #   {:ok, []}
  # end

  # def side_load(_resource, record_or_records, [], _api, _global_params),
  #   do: {:ok, record_or_records}

  # def side_load(
  #       resource,
  #       %Ash.Actions.Paginator{results: results} = paginator,
  #       side_loads,
  #       api,
  #       global_params
  #     ) do
  #   case side_load(resource, results, side_loads, api, global_params) do
  #     {:ok, side_loaded} -> {:ok, %{paginator | results: side_loaded}}
  #     {:error, error} -> {:error, error}
  #   end
  # end

  # def side_load(resource, record, side_loads, api, global_params)
  #     when not is_list(record) do
  #   case side_load(resource, [record], side_loads, api, global_params) do
  #     {:ok, [side_loaded]} -> {:ok, side_loaded}
  #     {:error, error} -> {:error, error}
  #   end
  # end

  # def side_load(resource, records, side_loads, api, global_params) do
  #   {side_load_type, config} = Ash.side_load_config(api)

  #   side_loads = sanitize_side_loads(side_loads)

  #   side_load_results =
  #     side_loads
  #     |> maybe_async_stream(config, async?, fn relationship_name, further ->
  #       relationship = Ash.relationship(resource, relationship_name)

  #       # Combining filters, and handling boolean filters is
  #       # going to come into play here. #TODO

  #       # need to be able to configure options specific to the path of the preload!
  #       unless relationship.reverse_relationship do
  #         raise "no reverse relationship for #{inspect(relationship)}. This should be validated at compile time."
  #       end

  #       action_params =
  #         global_params
  #         |> Keyword.put(
  #           :filter,
  #           [{relationship.reverse_relationship, reverse_relationship_filter(records)}]
  #         )
  #         |> Keyword.put_new(:paginate?, false)

  #       with {:ok, %{results: related_records}} <-
  #              api.read(relationship.destination, action_params),
  #            {:ok, side_loaded_related} <-
  #              side_load(relationship.destination, related_records, further, api, global_params) do
  #         keyed_by_id =
  #           Enum.group_by(side_loaded_related, fn record ->
  #             # This is required for many to many relationships
  #             Map.get(record, :__related_id__) ||
  #               Map.get(record, relationship.destination_field)
  #           end)

  #         {:ok, {relationship, keyed_by_id}}
  #       else
  #         {:error, error} -> {:error, error}
  #       end
  #     end)
  #     |> Enum.to_list()

  #   # This is dumb, should handle these errors better
  #   first_error =
  #     Enum.find(side_load_results, fn side_loaded ->
  #       match?({:error, _error}, side_loaded)
  #     end)

  #   if first_error do
  #     first_error
  #   else
  #     {:ok, link_records(Enum.map(side_load_results, &elem(&1, 1)), records)}
  #   end
  # end

  defp sanitize_side_loads(side_loads) do
    Enum.map(side_loads, fn side_load_part ->
      if is_atom(side_load_part) do
        {side_load_part, []}
      else
        side_load_part
      end
    end)
  end

  # defp reverse_relationship_filter(records) when is_list(records) do
  #   [or: records |> List.wrap() |> Enum.map(&reverse_relationship_filter/1)]
  # end

  # defp reverse_relationship_filter(%resource{} = record) do
  #   record |> Map.take(Ash.primary_key(resource)) |> Map.to_list()
  # end

  # defp link_records(results, records) do
  #   Enum.reduce(results, records, fn {relationship, keyed_by_id}, records ->
  #     Enum.map(records, fn record ->
  #       related_to_this_record =
  #         Map.get(keyed_by_id, Map.get(record, relationship.source_field)) || []

  #       unwrapped =
  #         if relationship.cardinality == :many do
  #           related_to_this_record
  #         else
  #           List.first(related_to_this_record)
  #         end

  #       related_ids = Enum.map(related_to_this_record, fn record -> record.id end)

  #       linked_record =
  #         record
  #         |> Map.put(relationship.name, unwrapped)
  #         |> Map.put_new(:__linkage__, %{})
  #         |> Map.update!(:__linkage__, &Map.put(&1, relationship.name, related_ids))

  #       linked_record
  #     end)
  #   end)
  # end

  # defp maybe_async_stream(preloads, _opts, false, function) do
  #   Stream.map(preloads, fn {association, further} ->
  #     function.(association, further)
  #   end)
  # end

  # defp maybe_async_stream(preloads, opts, true, function) do
  #   # We could theoretically do one of them outside of a task whlie we wait for the rest
  #   # Not worth implementing to start, IMO.
  #   async_opts = [
  #     opts[:max_concurrency] || System.schedulers_online(),
  #     ordered: false,
  #     timeout: opts[:timeout] || :timer.seconds(5),
  #     on_timeout: :kill_task,
  #     shutdown: opts[:shutdown] || :timer.seconds(5)
  #   ]

  #   Task.Supervisor.async_stream_nolink(
  #     opts[:supervisor],
  #     preloads,
  #     fn {key, further} -> function.(key, further) end,
  #     async_opts
  #   )
  #   |> Stream.map(&to_result/1)
  # end

  # defp to_result({:exit, reason}), do: {:error, {:exit, reason}}
  # defp to_result({:ok, {:ok, value}}), do: {:ok, value}
  # defp to_result({:ok, {:error, error}}), do: {:error, error}
end
