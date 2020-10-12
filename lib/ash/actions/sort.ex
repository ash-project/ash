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

  def sorting_on_identity?(%{sort: nil}), do: false

  def sorting_on_identity?(query) do
    identity_keys =
      query.resource
      |> Ash.Resource.identities()
      |> Enum.map(& &1.keys)

    sort_fields = Keyword.keys(query.sort)

    Enum.any?([Ash.Resource.primary_key(query.resource) | identity_keys], fn keyset ->
      Enum.all?(keyset, &(&1 in sort_fields))
    end)
  end

  def reverse(sort) do
    Enum.map(sort, fn {field, direction} ->
      case direction do
        :asc ->
          {field, :desc}

        :desc ->
          {field, :asc}
      end
    end)
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
    sort_by(results, &Map.get(&1, field), direction)
  end

  def runtime_sort(results, [{field, direction} | rest]) do
    results
    |> Enum.group_by(&Map.get(&1, field))
    |> sort_by(fn {key, _value} -> key end, direction)
    |> Enum.flat_map(fn {_, records} ->
      runtime_sort(records, rest)
    end)
  end

  # :asc/:desc added to elixir in 1.10. sort_by and to_sort_by_fun copied from core
  defp sort_by(enumerable, mapper, sorter) do
    enumerable
    |> Enum.map(&{&1, mapper.(&1)})
    |> Enum.sort(to_sort_by_fun(sorter))
    |> Enum.map(&elem(&1, 0))
  end

  defp to_sort_by_fun(sorter) when is_function(sorter, 2),
    do: &sorter.(elem(&1, 1), elem(&2, 1))

  defp to_sort_by_fun(:asc),
    do: &(elem(&1, 1) <= elem(&2, 1))

  defp to_sort_by_fun(:desc),
    do: &(elem(&1, 1) >= elem(&2, 1))

  defp to_sort_by_fun(module) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :gt)

  defp to_sort_by_fun({:asc, module}) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :gt)

  defp to_sort_by_fun({:desc, module}) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :lt)
end
