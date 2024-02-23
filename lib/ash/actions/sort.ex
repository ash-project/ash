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

  def process(resource, sort, aggregates, context) do
    sort
    |> List.wrap()
    |> Enum.map(fn
      {key, {order, context}} when is_atom(order) ->
        {key, {order, context}}

      {key, val} ->
        if is_atom(val) do
          {key, val}
        else
          {key, {:asc, val}}
        end

      val ->
        {val, :asc}
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

            Code.ensure_compiled!(module)

            if function_exported?(module, :expression, 2) do
              if Ash.DataLayer.data_layer_can?(resource, :expression_calculation_sort) do
                calculation_sort(
                  field,
                  calc,
                  module,
                  opts,
                  calc.type,
                  calc.constraints,
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
            aggregate_sort(aggregate, order, resource, sorts, errors)

          Map.has_key?(aggregates, field) ->
            aggregate_sort(Map.get(aggregates, field), order, resource, sorts, errors)

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
                  calc.constraints,
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
            {sorts, [NoSuchAttribute.exception(name: field, resource: resource) | errors]}

          Ash.Type.embedded_type?(attribute.type) ->
            {sorts, ["Cannot sort on embedded types" | errors]}

          match?({:array, _}, attribute.type) ->
            {sorts, ["Cannot sort on array types" | errors]}

          !Ash.DataLayer.data_layer_can?(
            resource,
            {:sort, Ash.Type.storage_type(attribute.type, attribute.constraints)}
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

    count_of_sort = Enum.count(query.sort)

    Enum.any?([Ash.Resource.Info.primary_key(query.resource) | identity_keys], fn keyset ->
      last_n_fields = query.sort |> Enum.reverse() |> Enum.take(count_of_sort)

      Enum.all?(keyset, fn key ->
        Enum.any?(last_n_fields, fn
          {sort, _} when is_atom(sort) ->
            sort == key

          _ ->
            false
        end)
      end)
    end)
  end

  defp aggregate_sort(field, order, resource, sorts, errors) do
    {field, type} =
      case field do
        %{name: name, kind: :custom, type: type} ->
          {name, type}

        %Ash.Resource.Aggregate{} = agg ->
          attribute =
            if agg.field do
              related = Ash.Resource.Info.related(resource, agg.relationship_path)
              Ash.Resource.Info.attribute(related, agg.field)
            end

          attribute_type =
            if attribute do
              attribute.type
            end

          attribute_constraints =
            if attribute do
              attribute.constraints
            end

          {agg.name,
           Ash.Query.Aggregate.kind_to_type(agg.kind, attribute_type, attribute_constraints)}
      end

    case type do
      {:ok, type, _constraints} ->
        if Ash.DataLayer.data_layer_can?(resource, :aggregate_sort) &&
             Ash.DataLayer.data_layer_can?(
               resource,
               # do we need to get actual constraints for aggregates here?
               {:sort, Ash.Type.storage_type(type, [])}
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

  defp calculation_sort(
         field,
         calc,
         module,
         opts,
         type,
         constraints,
         order,
         sorts,
         errors,
         context
       ) do
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
             constraints,
             arguments: input,
             filterable?: calc.filterable?,
             load: calc.load,
             source_context: context
           ) do
      calc = Map.put(calc, :load, field)
      {sorts ++ [{calc, order}], errors}
    else
      {:error, error} ->
        {sorts, [error | errors]}
    end
  end

  @doc """
  Sort records at runtime

  Opts

  * `:api` - The api to use if data needs to be loaded
  * `:lazy?` - Whether to use already loaded values or to re-load them when necessary. Defaults to `false`
  """
  def runtime_sort(results, sort, opts \\ [])
  def runtime_sort([], _empty, _), do: []
  def runtime_sort(results, empty, _) when empty in [nil, []], do: results
  def runtime_sort([single_result], _, _), do: [single_result]

  def runtime_sort([%resource{} | _] = results, [{field, direction} | rest], opts) do
    results
    |> load_field(field, resource, opts)
    |> Enum.group_by(&resolve_field(&1, field, resource, api: opts))
    |> Enum.sort_by(fn {key, _value} -> key end, to_sort_by_fun(direction))
    |> Enum.flat_map(fn {_, records} ->
      runtime_sort(records, rest, Keyword.put(opts, :rekey?, false))
    end)
    |> maybe_rekey(results, resource, Keyword.get(opts, :rekey?, true))
  end

  defp maybe_rekey(new_results, results, resource, true) do
    Enum.map(new_results, fn new_result ->
      Enum.find(results, fn result ->
        resource.primary_key_matches?(new_result, result)
      end)
    end)
  end

  defp maybe_rekey(new_results, _, _, _), do: new_results

  def runtime_distinct(results, sort, opts \\ [])
  def runtime_distinct([], _empty, _), do: []
  def runtime_distinct(results, empty, _) when empty in [nil, []], do: results
  def runtime_distinct([single_result], _, _), do: [single_result]

  def runtime_distinct([%resource{} | _] = results, [{field, direction} | rest], opts) do
    results
    |> load_field(field, resource, opts)
    |> Enum.group_by(&resolve_field(&1, field, resource, api: opts))
    |> Enum.sort_by(fn {key, _value} -> key end, to_sort_by_fun(direction))
    |> Enum.map(fn {_key, [first | _]} ->
      first
    end)
    |> runtime_distinct(rest, Keyword.put(opts, :rekey?, false))
    |> maybe_rekey(results, resource, Keyword.get(opts, :rekey?, true))
  end

  defp load_field(records, field, resource, opts) do
    if is_nil(opts[:api]) || (opts[:lazy?] && Ash.Resource.loaded?(records, field)) do
      records
    else
      query =
        resource
        |> Ash.Query.load(field)
        |> Ash.Query.set_context(%{private: %{internal?: true}})

      opts[:api].load!(records, query)
    end
  end

  defp resolve_field(record, %Ash.Query.Calculation{} = calc, resource, opts) do
    cond do
      calc.module.has_calculate?() ->
        context = Map.put(calc.context, :api, opts[:api])

        case calc.module.calculate([record], calc.opts, context) do
          {:ok, [value]} -> value
          _ -> nil
        end

      calc.module.has_expression?() ->
        expression = calc.module.expression(calc.opts, calc.context)

        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, expression} ->
            case Ash.Expr.eval_hydrated(expression, record: record, resource: resource) do
              {:ok, value} ->
                value

              _ ->
                nil
            end

          _ ->
            nil
        end

      true ->
        nil
    end
    |> case do
      %Ash.ForbiddenField{} -> nil
      other -> other
    end
  end

  defp resolve_field(record, field, _resource, _) do
    record
    |> Map.get(field)
    |> case do
      %Ash.ForbiddenField{} -> nil
      other -> other
    end
  end

  defp to_sort_by_fun(:desc) do
    to_sort_by_fun(:desc_nils_first)
  end

  defp to_sort_by_fun(:asc) do
    to_sort_by_fun(:asc_nils_last)
  end

  defp to_sort_by_fun(:asc_nils_last) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          false

        is_nil(y) ->
          true

        true ->
          Comp.less_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_last) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          false

        is_nil(y) ->
          true

        true ->
          Comp.greater_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:asc_nils_first) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          true

        is_nil(y) ->
          false

        true ->
          Comp.less_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_first) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          true

        is_nil(y) ->
          false

        true ->
          Comp.greater_or_equal?(x, y)
      end
    end
  end
end
