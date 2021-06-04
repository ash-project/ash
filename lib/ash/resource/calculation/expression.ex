defmodule Ash.Resource.Calculation.Expression do
  @moduledoc false
  use Ash.Calculation, type: :string

  def expression(opts, context) do
    expr =
      Ash.Filter.build_filter_from_template(opts[:expr], nil, context, context[:context] || %{})

    Ash.Filter.build_filter_from_template(expr, nil, context, context[:context] || %{})
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

        names = Enum.uniq(aggs_from_calcs ++ aggs_from_this_calc)

        Ash.Query.load(query, names)

      {:error, _} ->
        query
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
        |> Enum.filter(&(&1.relationship_path != []))
        |> Enum.filter(&match?(%{attribute: %Ash.Resource.Attribute{}}, &1))
        |> Enum.map(& &1.attribute.name)
    end
  end
end
