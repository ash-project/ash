defmodule Ash.Actions.SideLoad do
  def process(resource, side_load, source_filter, path \\ [])
  def process(_, [], _, _), do: {:ok, []}

  def process(resource, side_load, source_filter, path) do
    # TODO: return authorizations here.
    Enum.reduce(side_load, {:ok, []}, fn
      _, {:error, error} ->
        {:error, error}

      {key, true}, {:ok, acc} ->
        do_process(resource, key, [], source_filter, path, acc)

      {key, further}, {:ok, acc} ->
        do_process(resource, key, further, source_filter, path, acc)

      key, {:ok, acc} ->
        do_process(resource, key, [], source_filter, path, acc)
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

  defp do_process(resource, key, further, source_filter, path, acc) do
    with {:rel, relationship} when not is_nil(relationship) <-
           {:rel, Ash.relationship(resource, key)},
         nested_path <- path ++ [key],
         {:ok, authorizations} <-
           process(relationship.destination, further, source_filter, nested_path) do
      filter = put_nested_relationship(nested_path, source_filter)
      auth = {resource, :read, [filter: filter]}

      {:ok, [auth | authorizations] ++ acc}
    else
      {:rel, nil} -> {:error, "no such relationship: #{key}"}
      {:error, error} -> {:error, error}
    end
  end

  defp put_nested_relationship([rel | rest], value) do
    [
      {rel, put_nested_relationship(rest, value)}
    ]
  end

  defp put_nested_relationship([], value) do
    value
  end

  def side_load(resource, record, keyword, api, global_params \\ [])

  def side_load(_resource, [], _side_loads, _api, _global_params) do
    {:ok, []}
  end

  def side_load(_resource, record_or_records, [], _api, _global_params),
    do: {:ok, record_or_records}

  def side_load(
        resource,
        %Ash.Actions.Paginator{results: results} = paginator,
        side_loads,
        api,
        global_params
      ) do
    case side_load(resource, results, side_loads, api, global_params) do
      {:ok, side_loaded} -> {:ok, %{paginator | results: side_loaded}}
      {:error, error} -> {:error, error}
    end
  end

  def side_load(resource, record, side_loads, api, global_params)
      when not is_list(record) do
    case side_load(resource, [record], side_loads, api, global_params) do
      {:ok, [side_loaded]} -> {:ok, side_loaded}
      {:error, error} -> {:error, error}
    end
  end

  def side_load(resource, records, side_loads, api, global_params) do
    {side_load_type, config} = Ash.side_load_config(api)
    async? = side_load_type == :parallel

    side_loads = sanitize_side_loads(side_loads)

    side_load_results =
      side_loads
      |> maybe_async_stream(config, async?, fn relationship_name, further ->
        relationship = Ash.relationship(resource, relationship_name)

        # Combining filters, and handling boolean filters is
        # going to come into play here. #TODO

        # need to be able to configure options specific to the path of the preload!
        unless relationship.reverse_relationship do
          raise "no reverse relationship for #{inspect(relationship)}. This should be validated at compile time."
        end

        action_params =
          global_params
          |> Keyword.put(
            :filter,
            [{relationship.reverse_relationship, reverse_relationship_filter(records)}]
          )
          |> Keyword.put_new(:paginate?, false)

        with {:ok, %{results: related_records}} <-
               api.read(relationship.destination, action_params),
             {:ok, side_loaded_related} <-
               side_load(relationship.destination, related_records, further, api, global_params) do
          keyed_by_id =
            Enum.group_by(side_loaded_related, fn record ->
              # This is required for many to many relationships
              Map.get(record, :__related_id__) ||
                Map.get(record, relationship.destination_field)
            end)

          {:ok, {relationship, keyed_by_id}}
        else
          {:error, error} -> {:error, error}
        end
      end)
      |> Enum.to_list()

    # This is dumb, should handle these errors better
    first_error =
      Enum.find(side_load_results, fn side_loaded ->
        match?({:error, _error}, side_loaded)
      end)

    if first_error do
      first_error
    else
      {:ok, link_records(Enum.map(side_load_results, &elem(&1, 1)), records)}
    end
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

  defp reverse_relationship_filter(records) when is_list(records) do
    [or: records |> List.wrap() |> Enum.map(&reverse_relationship_filter/1)]
  end

  defp reverse_relationship_filter(%resource{} = record) do
    record |> Map.take(Ash.primary_key(resource)) |> Map.to_list()
  end

  defp link_records(results, records) do
    Enum.reduce(results, records, fn {relationship, keyed_by_id}, records ->
      Enum.map(records, fn record ->
        related_to_this_record =
          Map.get(keyed_by_id, Map.get(record, relationship.source_field)) || []

        unwrapped =
          if relationship.cardinality == :many do
            related_to_this_record
          else
            List.first(related_to_this_record)
          end

        related_ids = Enum.map(related_to_this_record, fn record -> record.id end)

        linked_record =
          record
          |> Map.put(relationship.name, unwrapped)
          |> Map.put_new(:__linkage__, %{})
          |> Map.update!(:__linkage__, &Map.put(&1, relationship.name, related_ids))

        linked_record
      end)
    end)
  end

  defp maybe_async_stream(preloads, _opts, false, function) do
    Stream.map(preloads, fn {association, further} ->
      function.(association, further)
    end)
  end

  defp maybe_async_stream(preloads, opts, true, function) do
    # We could theoretically do one of them outside of a task whlie we wait for the rest
    # Not worth implementing to start, IMO.
    async_opts = [
      opts[:max_concurrency] || System.schedulers_online(),
      ordered: false,
      timeout: opts[:timeout] || :timer.seconds(5),
      on_timeout: :kill_task,
      shutdown: opts[:shutdown] || :timer.seconds(5)
    ]

    Task.Supervisor.async_stream_nolink(
      opts[:supervisor],
      preloads,
      fn {key, further} -> function.(key, further) end,
      async_opts
    )
    |> Stream.map(&to_result/1)
  end

  defp to_result({:exit, reason}), do: {:error, {:exit, reason}}
  defp to_result({:ok, {:ok, value}}), do: {:ok, value}
  defp to_result({:ok, {:error, error}}), do: {:error, error}
end
