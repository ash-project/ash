# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Calculation.Expression do
  @moduledoc false
  use Ash.Resource.Calculation

  def expression(opts, context) do
    expr =
      Ash.Expr.fill_template(
        opts[:expr],
        actor: context.actor,
        tenant: context.tenant,
        args: context.arguments,
        context: context.source_context
      )

    if type = context.type do
      case expr do
        %Ash.Query.Function.Type{arguments: [_, ^type | _]} ->
          expr

        %Ash.Query.Call{name: :type, args: [_, ^type | _]} ->
          expr

        _ ->
          {:ok, expr} =
            Ash.Query.Function.Type.new([
              expr,
              context.type,
              context.constraints || []
            ])

          expr
      end
    else
      expr
    end
  end

  def calculate([], _, _), do: []

  def calculate([%resource{} | _] = records, opts, context) do
    expression =
      Ash.Expr.fill_template(
        opts[:expr],
        actor: context.actor,
        tenant: context.tenant,
        args: context.arguments,
        context: context.source_context || %{}
      )

    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, values} ->
      case Ash.Filter.hydrate_refs(expression, %{
             resource: resource,
             public?: false,
             eval?: true
           }) do
        {:ok, expression} ->
          case Ash.Expr.eval_hydrated(expression,
                 record: record,
                 resource: resource,
                 actor: context.actor,
                 unknown_on_unknown_refs?: true
               ) do
            {:ok, value} ->
              value = try_cast_stored(value, context.type, context.constraints)
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

  def try_cast_stored(value, nil, _constraints), do: value

  def try_cast_stored(value, type, constraints) do
    case Ash.Type.cast_stored(type, value, constraints || []) do
      {:ok, value} -> value
      _ -> value
    end
  end

  def load(query, opts, context) do
    expr =
      Ash.Expr.fill_template(
        opts[:expr],
        actor: context.actor,
        tenant: context.tenant,
        args: context.arguments,
        context: context.source_context || %{}
      )

    case Ash.Filter.hydrate_refs(expr, %{
           resource: query.resource,
           calculations: query.calculations,
           aggregates: query.aggregates,
           public?: false,
           eval?: true
         }) do
      {:ok, expression} ->
        expression
        |> Ash.Filter.list_refs()
        |> Enum.reduce([], fn
          %{attribute: %Ash.Query.Aggregate{} = agg, relationship_path: relationship_path}, acc ->
            add_at_path(acc, relationship_path, agg)

          %{attribute: %Ash.Query.Calculation{} = calc, relationship_path: relationship_path},
          acc ->
            add_at_path(acc, relationship_path, calc)

          %{attribute: %{name: name}, relationship_path: relationship_path}, acc ->
            add_at_path(acc, relationship_path, name)
        end)
        |> Enum.concat(Ash.Filter.used_aggregates(expression))
        |> Enum.uniq()

      {:error, _} ->
        []
    end
  end

  defp add_at_path(acc, [], value) do
    Enum.uniq([value | acc])
  end

  defp add_at_path(acc, [first | rest], value) do
    Enum.reduce(acc, {acc, false}, fn
      {key, current}, {acc, _found?} when key == first ->
        {[{key, add_at_path(List.wrap(current), rest, value)} | acc], true}

      key, {acc, _found?} when key == first ->
        {[{key, add_at_path([], rest, value)} | acc], true}

      v, {acc, found?} ->
        {[v | acc], found?}
    end)
    |> case do
      {acc, false} ->
        [{first, add_at_path([], rest, value)} | acc]

      {acc, _} ->
        acc
    end
  end
end
