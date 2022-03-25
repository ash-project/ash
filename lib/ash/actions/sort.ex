defmodule Ash.Actions.Sort do
  @moduledoc false
  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidSortOrder,
    NoSuchAttribute,
    UnsortableAttribute
  }

  @sort_orders [:asc, :desc, :asc_nils_first, :asc_nils_last, :desc_nils_first, :desc_nils_last]

  def process(_resource, empty, _aggregates, context \\ %{})

  def process(_resource, empty, _aggregates, _context) when empty in [nil, []], do: {:ok, []}

  def process(resource, sort, aggregates, context) when is_list(sort) do
    sort
    |> Enum.map(fn
      {key, {order, context}} when is_atom(order) ->
        {key, {order, context}}

      {key, val} ->
        if is_atom(val) do
          {key, val}
        else
          {key, {:asc, val}}
        end
    end)
    |> Enum.reduce({[], []}, fn
      {field, {inner_order, _} = order}, {sorts, errors} when inner_order in @sort_orders ->
        case Ash.Resource.Info.calculation(resource, field) do
          nil ->
            {sorts,
             [
               "Cannot provide context to a non-calculation field while sorting"
               | errors
             ]}

          calc ->
            {module, opts} = calc.calculation
            Code.ensure_compiled(module)

            if :erlang.function_exported(module, :expression, 2) do
              if Ash.DataLayer.data_layer_can?(resource, :expression_calculation_sort) do
                calculation_sort(
                  field,
                  calc,
                  module,
                  opts,
                  calc.type,
                  order,
                  sorts,
                  errors,
                  context
                )
              else
                {sorts, ["Datalayer cannot sort on calculations"]}
              end
            else
              {sorts, ["Calculations cannot be sorted on unless they define an expression"]}
            end
        end

      {%Ash.Query.Calculation{} = calc, order}, {sorts, errors} ->
        if order in @sort_orders do
          {sorts ++ [{calc, order}], errors}
        else
          {sorts, [InvalidSortOrder.exception(order: order) | errors]}
        end

      {field, order}, {sorts, errors} when order in @sort_orders ->
        attribute = Ash.Resource.Info.attribute(resource, field)

        cond do
          aggregate = Ash.Resource.Info.aggregate(resource, field) ->
            aggregate_sort(aggregates, aggregate, order, resource, sorts, errors)

          Map.has_key?(aggregates, field) ->
            aggregate_sort(aggregates, field, order, resource, sorts, errors)

          calc = Ash.Resource.Info.calculation(resource, field) ->
            {module, opts} = calc.calculation
            Code.ensure_compiled(module)

            if :erlang.function_exported(module, :expression, 2) do
              if Ash.DataLayer.data_layer_can?(resource, :expression_calculation_sort) do
                calculation_sort(
                  field,
                  calc,
                  module,
                  opts,
                  calc.type,
                  order,
                  sorts,
                  errors,
                  context
                )
              else
                {sorts, ["Datalayer cannot sort on calculations"]}
              end
            else
              {sorts, ["Calculations cannot be sorted on unless they define an expression"]}
            end

          !attribute ->
            {sorts, [NoSuchAttribute.exception(name: field) | errors]}

          Ash.Type.embedded_type?(attribute.type) ->
            {sorts, ["Cannot sort on embedded types" | errors]}

          match?({:array, _}, attribute.type) ->
            {sorts, ["Cannot sort on array types" | errors]}

          !Ash.DataLayer.data_layer_can?(
            resource,
            {:sort, Ash.Type.storage_type(attribute.type)}
          ) ->
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
      |> Ash.Resource.Info.identities()
      |> Enum.map(& &1.keys)

    sort_fields = Keyword.keys(query.sort)

    Enum.any?([Ash.Resource.Info.primary_key(query.resource) | identity_keys], fn keyset ->
      Enum.all?(keyset, &(&1 in sort_fields))
    end)
  end

  defp aggregate_sort(aggregates, field, order, resource, sorts, errors) do
    {field, type} =
      case field do
        field when is_atom(field) ->
          aggregate = Map.get(aggregates, field)

          {field, {:ok, aggregate.type}}

        %Ash.Resource.Aggregate{} = agg ->
          field_type =
            if agg.field do
              related = Ash.Resource.Info.related(resource, agg.relationship_path)
              Ash.Resource.Info.attribute(related, agg.field).type
            end

          {agg.name, Ash.Query.Aggregate.kind_to_type(agg.kind, field_type)}
      end

    case type do
      {:ok, type} ->
        if Ash.DataLayer.data_layer_can?(resource, :aggregate_sort) &&
             Ash.DataLayer.data_layer_can?(
               resource,
               {:sort, Ash.Type.storage_type(type)}
             ) do
          {sorts ++ [{field, order}], errors}
        else
          {sorts,
           [AggregatesNotSupported.exception(resource: resource, feature: "sorting") | errors]}
        end

      {:error, error} ->
        {sorts, [error | errors]}
    end
  end

  defp calculation_sort(field, calc, module, opts, type, order, sorts, errors, context) do
    {order, calc_context} =
      case order do
        order when is_atom(order) ->
          {order, %{}}

        {order, value} when is_list(value) ->
          {order, Map.new(value)}

        {order, value} when is_map(value) ->
          {order, value}

        other ->
          {other, %{}}
      end

    with {:ok, input} <- Ash.Query.validate_calculation_arguments(calc, calc_context),
         {:ok, calc} <-
           Ash.Query.Calculation.new(
             field,
             module,
             opts,
             type,
             Map.put(input, :context, context)
           ) do
      {sorts ++ [{calc, order}], errors}
    else
      {:error, error} ->
        {sorts, [error | errors]}
    end
  end

  def runtime_sort([], _empty), do: []
  def runtime_sort(results, empty) when empty in [nil, []], do: results

  def runtime_sort([%resource{} | _] = results, [{field, direction}]) do
    sort_by(results, &resolve_field(&1, field, resource), direction)
  end

  def runtime_sort([%resource{} | _] = results, [{field, direction} | rest]) do
    results
    |> Enum.group_by(&resolve_field(&1, field, resource))
    |> sort_by(fn {key, _value} -> key end, direction)
    |> Enum.flat_map(fn {_, records} ->
      runtime_sort(records, rest)
    end)
  end

  defp resolve_field(record, %Ash.Query.Calculation{} = calc, resource) do
    cond do
      :erlang.function_exported(calc.module, :calculate, 3) ->
        calc.module.calculate([record], calc.opts, calc.context)

      :erlang.function_exported(calc.module, :expression, 2) ->
        expression = calc.module.expression(calc.opts, calc.context)

        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, expression} ->
            case Ash.Filter.Runtime.do_match(record, expression) do
              {:ok, value} ->
                {:ok, value}

              _ ->
                nil
            end

          _ ->
            nil
        end

      true ->
        nil
    end
  end

  defp resolve_field(record, field, _resource) do
    Map.get(record, field)
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

  defp to_sort_by_fun(:asc) do
    fn
      nil, nil ->
        true

      _, nil ->
        true

      nil, _ ->
        false

      x, y ->
        Comp.less_or_equal?(elem(x, 1), elem(y, 1))
    end
  end

  defp to_sort_by_fun(:desc) do
    fn
      nil, nil ->
        true

      _, nil ->
        false

      nil, _ ->
        true

      x, y ->
        Comp.greater_or_equal?(elem(x, 1), elem(y, 1))
    end
  end

  defp to_sort_by_fun(:asc_nils_last) do
    fn x, y ->
      if is_nil(elem(x, 1)) && !is_nil(elem(y, 1)) do
        false
      else
        Comp.less_or_equal?(elem(x, 1), elem(y, 1))
      end
    end
  end

  defp to_sort_by_fun(:asc_nils_first) do
    fn x, y ->
      if is_nil(elem(x, 1)) && !is_nil(elem(y, 1)) do
        true
      else
        Comp.less_or_equal?(elem(x, 1), elem(y, 1))
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_first) do
    fn x, y ->
      if is_nil(elem(x, 1)) && !is_nil(elem(y, 1)) do
        true
      else
        Comp.greater_or_equal?(elem(x, 1), elem(y, 1))
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_last) do
    fn x, y ->
      if is_nil(elem(x, 1)) && !is_nil(elem(y, 1)) do
        false
      else
        Comp.greater_or_equal?(elem(x, 1), elem(y, 1))
      end
    end
  end

  defp to_sort_by_fun({direction, _input}) do
    to_sort_by_fun(direction)
  end

  defp to_sort_by_fun(module) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :gt)

  defp to_sort_by_fun({:asc, module}) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :gt)

  defp to_sort_by_fun({:desc, module}) when is_atom(module),
    do: &(module.compare(elem(&1, 1), elem(&2, 1)) != :lt)
end
