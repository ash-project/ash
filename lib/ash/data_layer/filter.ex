defmodule Ash.DataLayer.Filter do
  @filter_types [
    :equal
  ]

  @type filter_type :: :equal

  @spec filter_types() :: list(filter_type())
  def filter_types() do
    @filter_types
  end

  # This logic will need to get more complex as the ability to customize filter handling arises
  # as well as when complex filter types are added
  def process(resource, filter) do
    filter
    |> Enum.reduce({%{}, []}, fn {name, value}, {acc, errors} ->
      process_filter(resource, name, value, {acc, errors})
    end)
    |> case do
      {filter, []} -> {:ok, filter}
      {_, errors} -> {:error, errors}
    end
  end

  # TODO: Look into making `from_related` accept a full filter statement for the source entity,
  # so you can say `%{filter: %{from_related: %{owner: %{name: "zach"}}}}. This would let us optimize
  # and predict query results better, as well as represent the request to "get" those entities we
  # are filtering against as an ash request, so that authorization happens for free :D
  defp process_filter(_resource, :from_related, {[], relationship}, {filter, errors})
       when is_list(relationship) do
    {Map.put(filter, :__impossible__, true), errors}
  end

  defp process_filter(resource, :from_related, {related, relationship_name}, {filter, errors})
       when is_atom(relationship_name) do
    case Ash.relationship(resource, relationship_name) do
      nil ->
        {filter, ["no such relationship: #{relationship_name}" | errors]}

      relationship ->
        {Map.put(filter, :from_related, {related, relationship}), errors}
    end
  end

  defp process_filter(resource, field, value, {filter, errors}) do
    cond do
      attr = Ash.attribute(resource, field) ->
        process_attribute_filter(resource, attr, value, {filter, errors})

      rel = Ash.relationship(resource, field) ->
        process_relationship_filter(resource, rel, value, {filter, errors})

      true ->
        {filter, ["Unsupported filter: #{inspect(field)}" | errors]}
    end
  end

  defp process_attribute_filter(resource, %{name: name, type: type}, value, {filter, errors}) do
    with {:ok, casted} <- Ash.Type.cast_input(type, value),
         filters <- Ash.Type.supported_filter_types(type, Ash.data_layer(resource)),
         {:supported, true} <- {:supported, :equal in filters} do
      {Map.put(filter, name, casted), errors}
    else
      :error ->
        {filter, ["Invalid value: #{inspect(value)} for #{inspect(name)}" | errors]}

      {:supported, false} ->
        {filter, ["Cannot filter #{inspect(name)} for equality." | errors]}
    end
  end

  defp process_relationship_filter(_resource, %{name: name}, value, {filter, errors}) do
    # TODO: type validate, potentially expand list of ids into a boolean filter statement
    {Map.put(filter, name, value), errors}
  end
end
