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

  def value_to_primary_key_filter(resource, value) do
    do_value_to_primary_key_filter(resource, Ash.primary_key(resource), value)
  end

  defp do_value_to_primary_key_filter(_resource, [], _value), do: {:error, :no_primary_key}

  defp do_value_to_primary_key_filter(resource, primary_key, value) when is_map(value) do
    if Enum.all?(primary_key, &Map.has_key?(value, &1)) do
      value
      |> Map.take(primary_key)
      |> Enum.reduce({:ok, %{}}, fn
        {key, val}, {:ok, filter} ->
          attr = Ash.attribute(resource, key)

          case Ash.Type.cast_input(attr.type, val) do
            {:ok, casted} -> {:ok, Map.put(filter, attr.name, casted)}
            :error -> {:error, {key, "is invalid"}}
          end

        _, {:error, error} ->
          {:error, error}
      end)
    else
      {:error, "Invalid primary key"}
    end
  end

  defp do_value_to_primary_key_filter(resource, [field], value) do
    do_value_to_primary_key_filter(resource, [field], %{field => value})
  end

  defp do_value_to_primary_key_filter(_, _, _), do: {:error, ["Invalid primary key"]}

  # This logic will need to get more complex as the ability to customize filter handling arises
  # as well as when complex filter types are added
  def process(resource, filter) do
    state = %{errors: [], authorization: [], filter: []}

    filter
    |> Enum.reduce(state, fn {name, value}, state ->
      process_filter(resource, name, value, state)
    end)
    |> case do
      %{filter: filter, errors: [], authorization: authorization} -> {:ok, filter, authorization}
      %{errors: errors} -> {:error, errors}
    end
  end

  # TODO: Look into making `from_related` accept a full filter statement for the source entity,
  # so you can say `%{filter: [from_related: [owner: [name: "zach"]]]}. This would let us optimize
  # and predict query results better, as well as represent the request to "get" those entities we
  # are filtering against as an ash request, so that authorization happens for free :D
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
         {%_source_resource{}, %{type: :many_to_many} = _rel},
         state
       ) do
    add_error(state, "We don't support many to many filters yet")
  end

  defp process_filter(
         resource,
         :from_related,
         {related, rel},
         state
       ) do
    case related do
      [] ->
        process_filter(resource, rel.destination_field, [in: []], state)

      [related] ->
        process_filter(resource, rel.destination_field, Map.get(related, rel.source_field), state)

      [_ | _] = related ->
        values = Enum.map(related, &Map.get(&1, rel.source_field))

        process_filter(resource, rel.destination_field, [in: values], state)
    end
  end

  defp process_filter(resource, field, value, state) when not is_list(value) do
    process_filter(resource, field, [equal: value], state)
  end

  defp process_filter(resource, field, value, state) do
    cond do
      attr = Ash.attribute(resource, field) ->
        Enum.reduce(value, state, fn {key, val}, state ->
          do_process_filter(resource, attr.name, attr.type, key, val, state)
        end)

      rel = Ash.relationship(resource, field) ->
        case rel do
          %{type: :many_to_many} ->
            add_error(state, "no filtering on many to many")

          %{source_field: source_field} ->
            process_filter(resource, source_field, [equal: value], state)
        end

      true ->
        add_error(state, "unknown filter #{field}")
    end
  end

  defp do_process_filter(resource, field, field_type, filter_type, value, state) do
    with {:ok, casted} <- Ash.Type.cast_input(field_type, value),
         {:supported?, true} <- {:supported?, supports_filter?(resource, field_type, filter_type)} do
      %{state | filter: add_filter(state.filter, field, filter_type, casted)}
    else
      :error ->
        add_error(state, "Invalid value: #{inspect(value)} for #{inspect(field)}")

      {:supported?, false} ->
        add_error(state, "Cannot use filter type #{filter_type} on #{inspect(field)}.")
    end
  end

  defp supports_filter?(resource, type, filter_type) do
    Ash.Type.supports_filter?(type, filter_type, Ash.data_layer(resource))
  end

  defp add_filter(filter, field, :equal, value) do
    cond do
      colliding_equal_filter?(filter, field, value) ->
        filter
        |> Keyword.put(:__impossible__, true)
        |> Keyword.put(field, [{:equal, value} | filter[field]])

      colliding_in_filter?(filter, field, value) ->
        filter
        |> Keyword.put(:__impossible__, true)
        |> Keyword.put(field, [{:equal, value} | filter[field]])

      true ->
        Keyword.put(filter, field, equal: value)
    end
  end

  # defp process_relationship_filter(_resource, %{name: name}, value, {filter, errors}) do
  #   # TODO: type validate, potentially expand list of ids into a boolean filter statement
  #   {filter, ["no relationship filters" | errors]}
  # end

  defp colliding_equal_filter?(filter, name, casted) do
    case Keyword.fetch(filter, name) do
      :error ->
        false

      {:ok, filter} ->
        Enum.any?(filter, fn {key, value} ->
          key == :equal and value != casted
        end)
    end
  end

  defp colliding_in_filter?(filter, name, casted) do
    case Keyword.fetch(filter, name) do
      :error ->
        false

      {:ok, filter} ->
        Enum.any?(filter, fn {key, value} ->
          key == :in and casted not in value
        end)
    end
  end

  defp add_error(state, error), do: %{state | errors: [error | state.errors]}
end
