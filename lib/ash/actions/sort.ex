defmodule Ash.Actions.Sort do
  @moduledoc false
  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidSortOrder,
    NoSuchAttribute,
    UnsortableAttribute
  }

  def process(_resource, empty, _aggregates) when empty in [nil, []], do: {:ok, []}

  def process(resource, sort, aggregates) when is_list(sort) do
    sort
    |> Enum.reduce({[], []}, fn
      {field, order}, {sorts, errors} when order in [:asc, :desc] ->
        attribute = Ash.Resource.attribute(resource, field)

        cond do
          Map.has_key?(aggregates, field) ->
            aggregate_sort(aggregates, field, order, resource, sorts, errors)

          !attribute ->
            {sorts, [NoSuchAttribute.exception(attribute: field) | errors]}

          !Ash.Resource.data_layer_can?(resource, {:sort, Ash.Type.storage_type(attribute.type)}) ->
            {sorts,
             [
               UnsortableAttribute.exception(field: field)
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

  defp aggregate_sort(aggregates, field, order, resource, sorts, errors) do
    aggregate = Map.get(aggregates, field)

    if Ash.Resource.data_layer_can?(resource, :aggregate_sort) &&
         Ash.Resource.data_layer_can?(
           resource,
           {:sort, Ash.Type.storage_type(aggregate.type)}
         ) do
      {sorts ++ [{field, order}], errors}
    else
      {sorts, AggregatesNotSupported.exception(resource: resource, feature: "sorting")}
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
