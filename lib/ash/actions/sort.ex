defmodule Ash.Actions.Sort do
  @moduledoc false
  alias Ash.Error.Sort.{InvalidSortOrder, NoSuchField, UnsortableField}

  def process(_resource, empty) when empty in [nil, []], do: {:ok, []}

  def process(resource, sort) when is_list(sort) do
    sort
    |> Enum.reduce({[], []}, fn
      {field, order}, {sorts, errors} when order in [:asc, :desc] ->
        attribute = Ash.Resource.attribute(resource, field)

        cond do
          !attribute ->
            {sorts, [NoSuchField.exception(field: field) | errors]}

          !Ash.Resource.data_layer_can?(resource, {:sort, Ash.Type.storage_type(attribute.type)}) ->
            {sorts,
             [
               UnsortableField.exception(field: field)
               | errors
             ]}

          true ->
            {sorts ++ [{field, order}], errors}
        end

      {_, order}, {sorts, errors} ->
        {sorts, [InvalidSortOrder.exception(order: order) | errors]}
    end)
    |> case do
      {sorts, []} -> {:ok, sorts}
      {_, errors} -> {:error, errors}
    end
  end

  def runtime_sort(results, empty) when empty in [nil, []], do: results

  def runtime_sort(results, [{field, direction}]) do
    Enum.sort_by(results, &Map.get(&1, field), direction)
  end

  def runtime_sort(results, [{field, direction} | rest]) do
    results
    |> Enum.group_by(&Map.get(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end, direction)
    |> Enum.flat_map(fn {_, records} ->
      runtime_sort(records, rest)
    end)
  end
end
