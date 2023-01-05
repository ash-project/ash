defmodule Ash.Resource.Calculation.Expression do
  @moduledoc false
  use Ash.Calculation

  def expression(opts, context) do
    expr =
      Ash.Filter.build_filter_from_template(opts[:expr], nil, context, context[:context] || %{})

    Ash.Filter.build_filter_from_template(expr, nil, context, context[:context] || %{})
  end

  def calculate([], _, _), do: []

  def calculate([%resource{} | _] = records, opts, context) do
    expression =
      Ash.Filter.build_filter_from_template(opts[:expr], nil, context, context[:context] || %{})

    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, values} ->
      case Ash.Filter.hydrate_refs(expression, %{
             resource: resource,
             aggregates: %{},
             calculations: %{},
             public?: false
           }) do
        {:ok, expression} ->
          case Ash.Expr.eval_hydrated(expression, record: record) do
            {:ok, value} ->
              {:cont, {:ok, [value | values]}}

            :unknown ->
              {:halt, :unknown}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, values} ->
        {:ok, Enum.reverse(values)}

      :unknown ->
        :unknown

      {:error, error} ->
        {:error, error}
    end
  end

  def load(query, opts, context) do
    expr =
      Ash.Filter.build_filter_from_template(opts[:expr], nil, context, context[:context] || %{})

    case Ash.Filter.hydrate_refs(expr, %{
           resource: query.resource,
           calculations: query.calculations,
           aggregates: query.aggregates,
           public?: false
         }) do
      {:ok, expression} ->
        further_calculations =
          expression
          |> Ash.Filter.used_calculations(
            query.resource,
            query.calculations,
            query.aggregates
          )

        aggs_from_this_calc =
          expression
          |> Ash.Filter.used_aggregates()
          |> Enum.map(& &1.name)

        aggs_from_calcs =
          further_calculations
          |> Enum.flat_map(fn calculation ->
            calculation_context =
              calculation.context
              |> Map.put(:context, query.context)

            case Ash.Filter.hydrate_refs(
                   calculation.module.expression(calculation.opts, calculation_context),
                   %{
                     resource: query.resource,
                     calculations: query.calculations,
                     aggregates: query.aggregates,
                     public?: false
                   }
                 ) do
              {:ok, expression} ->
                expression
                |> Ash.Filter.used_aggregates()
                |> Enum.map(& &1.name)

              _ ->
                []
            end
          end)
          |> Enum.map(& &1.name)

        Enum.uniq(aggs_from_calcs ++ aggs_from_this_calc)

      {:error, _} ->
        []
    end
  end

  def select(query, opts, context) do
    expr =
      Ash.Filter.build_filter_from_template(opts[:expr], nil, context, context[:context] || %{})

    case Ash.Filter.hydrate_refs(expr, %{
           resource: query.resource,
           calculations: query.calculations,
           aggregates: query.aggregates,
           public?: false
         }) do
      {:ok, expression} ->
        expression
        |> Ash.Filter.list_refs()
        |> Enum.filter(fn ref ->
          ref.relationship_path == [] && match?(%Ash.Resource.Attribute{}, ref.attribute)
        end)
        |> Enum.map(& &1.attribute.name)
    end
  end
end
