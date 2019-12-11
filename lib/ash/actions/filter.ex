defmodule Ash.Actions.Filter do
  @filter_types [
    :equal
  ]

  @type filter_type :: :equal

  def is_filter_subset?(resource, source_filter, candidate) do
    candidate = process(resource, candidate)

    Enum.reduce(candidate, true, fn
      {candidate_key, candidate_value}, true ->
        case source_filter[candidate_key] do
          {:in, list} ->
            list_subset_of?(list, candidate_value)

          filter_value ->
            filter_value == candidate_value
        end

      _, false ->
        false
    end)
  end

  defp list_subset_of?(source_list, candidate_list) do
    candidate_set =
      candidate_list
      |> List.wrap()
      |> MapSet.new()

    Enum.all?(source_list, &MapSet.member?(candidate_set, &1))
  end

  @spec filter_types() :: list(filter_type())
  def filter_types() do
    @filter_types
  end

  # This logic will need to get more complex as the ability to customize filter handling arises
  # as well as when complex filter types are added
  def process(resource, filter) do
    state = %{errors: [], authorization: [], filter: [], joins: []}

    case filter_or_primary_key_filter(resource, filter) do
      {:ok, filter} ->
        filter
        |> Enum.reduce(state, fn {name, value}, state ->
          process_filter(resource, name, value, state)
        end)
        |> case do
          %{filter: filter, errors: [], authorization: authorization} ->
            {:ok, filter, authorization}

          %{errors: errors} ->
            {:error, errors}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  # From related does not need to authorize to read the source, because it
  # should only be called in cases where the source is present, and authorized
  # already, if necessary. We may want to support supplying a filter statement
  # over the destination as well.
  defp process_filter(resource, :from_related, {item, relationship}, state)
       when not is_list(item) do
    process_filter(resource, :from_related, {[item], relationship}, state)
  end

  defp process_filter(_resource, :from_related, {_, relationship}, state)
       when is_atom(relationship) do
    add_error(state, "Must provide relationship struct, not relationship name")
  end

  defp process_filter(
         _resource,
         :from_related,
         {source_records, %{type: :many_to_many, source: source} = rel},
         state
       ) do
    case records_to_primary_key_filter(source, source_records) do
      {:ok, filter} ->
        process_filter(
          source,
          rel.destination_field,
          {:join,
           [
             {rel.destination_field_on_join_table, {rel.through, []}},
             {rel.source_field, {rel.source, filter}}
           ]},
          state
        )

      {:error, error} ->
        add_error(state, error)
    end
  end

  defp process_filter(
         resource,
         :from_related,
         {related, rel},
         state
       ) do
    values = Enum.map(related, &Map.get(&1, rel.source_field))

    process_filter(resource, rel.destination_field, [in: values], state)
  end

  defp process_filter(
         resource,
         source_field,
         # Someday, I'll probably have to support sending additional information with the join
         {:join, {destination_field, destination, destination_filter}},
         state
       ) do
    with {:supports?, true} <- {:supports?, Ash.data_layer(resource).can?(:inner_join)},
         {:ok, destination_filter, authorization} <- process(destination, destination_filter) do
      state =
        state
        |> add_authorization(authorization)
        |> add_authorization({:read, destination, destination_filter})

      merge_join(state, source_field, destination_field, destination, destination_filter)
    else
      {:supports?, false} ->
        add_error(state, "Data layer does not support filtering")

      {:error, error} ->
        add_error(state, error)
    end
  end

  defp process_filter(resource, field, value, state) do
    cond do
      attr = Ash.attribute(resource, field) ->
        value =
          if Keyword.keyword?(value) do
            value
          else
            [equal: value]
          end

        Enum.reduce(value, state, fn
          {key, val}, state ->
            do_process_filter(resource, attr.name, attr.type, key, val, state)

          val, state ->
            do_process_filter(resource, attr.name, attr.type, :equal, val, state)
        end)

      rel = Ash.relationship(resource, field) ->
        case filter_or_primary_key_filter(rel.destination, value) do
          {:ok, filter} ->
            process_relationship_as_join(
              rel,
              state,
              filter,
              resource
            )

          {:error, error} ->
            add_error(state, error)
        end

      true ->
        add_error(state, "unknown filter #{resource}.#{field}: #{inspect(value)}")
    end
  end

  defp merge_join(
         %{joins: joins} = state,
         source_field,
         destination_field,
         destination,
         destination_filter
       ) do
    {new_fields, new_filter} =
      case Keyword.fetch(joins, destination) do
        {:ok, %{fields: fields, filter: filter}} ->
          merged_filter =
            Enum.reduce(destination_filter, filter, fn {key, value}, filter ->
              Keyword.update(filter, key, value, &Kernel.++(&1, value))
            end)

          new_fields =
            if {source_field, destination_field} in fields do
              fields
            else
              [{source_field, destination_field} | fields]
            end

          {new_fields, merged_filter}

        :error ->
          {[{source_field, destination_field}], destination_filter}
      end

    case process(destination, new_filter) do
      {:error, error} ->
        add_error(state, error)

      {:ok, processed, authorization} ->
        config = %{fields: new_fields, filter: processed}

        state
        |> add_authorization(authorization)
        |> Map.update!(:joins, &Keyword.put(&1, destination, config))
    end
  end

  defp filter_or_primary_key_filter(resource, value) do
    if Keyword.keyword?(value) do
      {:ok, value}
    else
      Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(resource, value)
    end
  end

  defp process_relationship_as_join(
         %{type: :many_to_many} = rel,
         state,
         destination_filter,
         resource
       ) do
    process_filter(
      resource,
      rel.source_field,
      {:join,
       {rel.source_field_on_join_table, rel.through,
        [
          {rel.destination_field_on_join_table,
           join: {rel.destination_field, rel.through, destination_filter}}
        ]}},
      state
    )
  end

  defp process_relationship_as_join(
         rel,
         state,
         destination_filter,
         resource
       ) do
    process_filter(
      resource,
      rel.source_field,
      {:join, {rel.destination_field, rel.destination, destination_filter}},
      state
    )
  end

  defp do_process_filter(resource, field, field_type, filter_type, value, state) do
    with {:ok, casted} <- cast_value(field_type, value),
         {:supported?, true} <- {:supported?, supports_filter?(resource, field_type, filter_type)} do
      %{state | filter: add_filter(state.filter, field, filter_type, casted)}
    else
      :error ->
        add_error(state, "Invalid value: #{inspect(value)} for #{inspect(field)}")

      {:supported?, false} ->
        add_error(state, "Cannot use filter type #{filter_type} on #{inspect(field)}.")
    end
  end

  defp records_to_primary_key_filter(resource, source_records) do
    case {Ash.primary_key(resource), source_records} do
      {[], _} ->
        {:error, "Can't build primary key"}

      {[primary_key], [source_record]} ->
        {:ok, [{primary_key, Map.get(source_record, primary_key)}]}

      {[primary_key], records} ->
        {:ok, [{primary_key, [in: Enum.map(records, &Map.get(&1, primary_key))]}]}

      {_, _records} ->
        {:error, "can't construction primary key filter for composite primary key"}
    end
  end

  defp cast_value(field_type, value) when is_list(value) do
    Enum.reduce(value, {:ok, []}, fn
      value, {:ok, casted_values} ->
        case Ash.Type.cast_input(field_type, value) do
          {:ok, casted_value} ->
            {:ok, [casted_value | casted_values]}

          :error ->
            :error
        end

      _value, :error ->
        :error
    end)
  end

  defp cast_value(field_type, value) do
    Ash.Type.cast_input(field_type, value)
  end

  defp supports_filter?(resource, type, filter_type) do
    Ash.Type.supports_filter?(type, filter_type, Ash.data_layer(resource))
  end

  defp add_filter(filter, field, filter_type, value) do
    current_value = filter[field] || []
    Keyword.put(filter, field, [{filter_type, value} | current_value])
  end

  defp add_error(state, errors) when is_list(errors),
    do: Enum.reduce(errors, state, &add_error(&2, &1))

  defp add_error(state, error), do: %{state | errors: [error | state.errors]}

  defp add_authorization(state, authorizations) when is_list(authorizations),
    do: Enum.reduce(authorizations, state, &add_authorization(&2, &1))

  defp add_authorization(state, authorization),
    do: %{state | authorization: [authorization | state.authorization]}
end
