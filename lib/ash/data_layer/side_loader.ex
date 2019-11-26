defmodule Ash.DataLayer.SideLoader do
  def side_load(resource, record, keyword, global_params \\ %{})

  def side_load(_resource, record_or_records, [], _global_params), do: {:ok, record_or_records}

  def side_load(resource, record, side_load_keyword, global_params) when not is_list(record) do
    case side_load(resource, [record], side_load_keyword, global_params) do
      {:ok, [side_loaded]} -> side_loaded
      {:error, error} -> {:error, error}
    end
  end

  def side_load(resource, records, side_load_keyword, global_params) do
    # TODO: No global config!
    config = Application.get_env(:ash, :side_loader)
    parallel_supervisor = config[:parallel_supervisor]

    side_loaded =
      side_load_keyword
      |> maybe_async_stream(config, parallel_supervisor, fn relationship_name, further ->
        relationship = Ash.relationship(resource, relationship_name)

        # Combining filters, and handling boolean filters is
        # going to come into play here. #TODO

        # need to be able to configure options specific to the path of the preload!
        action_params =
          global_params
          |> Map.put(:filter, %{
            from_related: {records, relationship}
          })
          |> Map.put_new(:paginate?, false)

        with {:ok, related_records} <- Ash.read(relationship.destination, action_params),
             {:ok, %{results: side_loaded_related}} <-
               side_load(relationship.destination, related_records, further, global_params) do
          # Somehow stitch them all together, the only way I can think of doing this is by making the `read`
          # action stitch everything together
          keyed_by_id =
            Enum.group_by(side_loaded_related, fn record ->
              Map.get(record, :__related_id__) ||
                Map.get(record, relationship.destination_field)
            end)

          Enum.map(records, fn record ->
            # This is required for many to many relationships
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
              |> Map.put(relationship_name, unwrapped)
              |> Map.put_new(:__linkage__, %{})
              |> Map.update!(:__linkage__, &Map.put(&1, relationship_name, related_ids))

            {:ok, linked_record}
          end)
        else
          {:error, error} -> {:error, error}
        end
      end)
      |> List.flatten()

    # This is dumb, should handle these errors better
    first_error =
      Enum.find(side_loaded, fn side_loaded ->
        match?({:error, _error}, side_loaded)
      end)

    first_error || {:ok, Enum.map(side_loaded, &elem(&1, 1))}
  end

  defp maybe_async_stream(preloads, _opts, nil, function) do
    Enum.map(preloads, fn {association, further} ->
      function.(association, further)
    end)
  end

  defp maybe_async_stream(preloads, opts, supervisor, function) do
    # We could theoretically do one of them outside of a task whlie we wait for the rest
    # Not worth implementing to start, IMO.
    opts = [
      opts[:max_concurrency] || System.schedulers_online(),
      ordered: false,
      timeout: opts[:timeout] || :timer.seconds(5),
      on_timeout: :kill_task,
      shutdown: opts[:shutdown] || :timer.seconds(5)
    ]

    supervisor
    |> Task.Supervisor.async_stream_nolink(
      preloads,
      fn {key, further} -> function.(key, further) end,
      opts
    )
    |> Stream.map(&to_result/1)
  end

  defp to_result({:exit, reason}), do: {:error, {:exit, reason}}
  defp to_result({:ok, {:ok, value}}), do: {:ok, value}
  defp to_result({:ok, {:error, error}}), do: {:error, error}
end
