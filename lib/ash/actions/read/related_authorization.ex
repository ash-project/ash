# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Read.RelatedAuthorization do
  @moduledoc false
  # Authorizes access to related data inside of expressions.
  #
  # Calculations with `authorize?: true` only see what the actor can see. When their expression
  # is expanded, every reference becomes `nil` unless the actor can read each related record on
  # the way to it, and can see the field itself. `exists` only considers related records that
  # the actor can read.

  alias Ash.Query.{BooleanExpression, Exists, Parent, Ref}

  @doc """
  Applies a calculation's `authorize?` option to its hydrated expression.

  This must be used anywhere that a calculation's expression is expanded to be evaluated, by Ash
  or by a data layer. The returned calculation no longer needs to be authorized.

  Options:

  - `:parent_stack` - the resources that `parent/1` refers to, innermost first
  - `:domain` - the domain to use for related resources that don't have one
  """
  @spec authorize_calculation(Ash.Query.Calculation.t(), term, Ash.Resource.t(), Keyword.t()) ::
          {Ash.Query.Calculation.t(), term}
  def authorize_calculation(calculation, expression, resource, opts \\ []) do
    if authorize_calculation?(calculation) do
      context = calculation.context
      domain = opts[:domain] || context.domain || Ash.Resource.Info.domain(resource)

      stack =
        Enum.map([resource | List.wrap(opts[:parent_stack])], &{&1, primary_read(&1)})

      {relationship_items, field_items} =
        expression
        |> requirements(stack)
        |> Enum.uniq()
        |> Enum.split_with(&(elem(&1, 0) in [:relationship, :unrelated_exists]))

      path_filters =
        case Ash.Filter.relationship_authorization_filters(
               domain,
               (context.source_context || %{})[:shared],
               context.actor,
               context.tenant,
               relationship_items,
               %{}
             ) do
          {:ok, path_filters} -> path_filters
          {:error, error} -> raise Ash.Error.to_ash_error(error)
        end

      field_conditions = field_conditions(field_items, domain, context)

      {%{calculation | authorize?: false},
       rewrite(expression, stack, path_filters, field_conditions)}
    else
      {calculation, expression}
    end
  end

  @doc """
  Prepares a relationship's filter for its related query.

  A relationship's definition is trusted, so calculations in its filter, and the calculations that
  they use, are not authorized. They are expanded up front so that nothing authorizes them later.
  """
  def trusted_filter(_resource, nil, _parent_stack), do: nil

  def trusted_filter(resource, filter, parent_stack) do
    parent_stack = List.wrap(parent_stack)

    case Ash.Filter.hydrate_refs(filter, %{
           resource: resource,
           parent_stack: parent_stack,
           public?: false
         }) do
      {:ok, filter} -> trust(filter, [resource | parent_stack])
      {:error, _} -> filter
    end
  end

  @doc """
  Prepares a relationship's sort for its related query. See `trusted_filter/3`.
  """
  def trusted_sort(_resource, sort) when sort in [nil, []], do: sort

  def trusted_sort(resource, sort) do
    case Ash.Query.sort(resource, sort) do
      %{errors: [], sort: sort} ->
        Enum.map(sort, fn
          {%Ash.Query.Calculation{} = calculation, order} ->
            {trust_calculation(calculation, [resource]), order}

          other ->
            other
        end)

      _ ->
        sort
    end
  end

  defp trust(expression, [resource | parents] = stack) do
    Ash.Filter.map(expression, fn
      %Ref{attribute: %Ash.Query.Calculation{} = calculation} = ref ->
        ref_resource = ref.resource || Ash.Resource.Info.related(resource, ref.relationship_path)
        {:halt, %{ref | attribute: trust_calculation(calculation, [ref_resource | parents])}}

      %Exists{related?: false, resource: destination, expr: expr} = exists ->
        {:halt, %{exists | expr: trust(expr, [destination | stack])}}

      %Exists{at_path: at_path, path: path, expr: expr} = exists ->
        destination = Ash.Resource.Info.related(resource, at_path ++ path)
        {:halt, %{exists | expr: trust(expr, [destination | stack])}}

      %Parent{expr: expr} = parent ->
        case parents do
          [_ | _] -> {:halt, %{parent | expr: trust(expr, parents)}}
          [] -> {:halt, parent}
        end

      other ->
        other
    end)
  end

  defp trust_calculation(calculation, [resource | parents] = stack) do
    calculation = %{calculation | authorize?: false}

    if Ash.Resource.Calculation.has_expression?(calculation.module) do
      calculation.module
      |> Ash.Resource.Calculation.expression(calculation.opts, calculation.context)
      |> Ash.Filter.hydrate_refs(%{resource: resource, parent_stack: parents, public?: false})
      |> case do
        {:ok, expression} ->
          %{
            calculation
            | module: Ash.Resource.Calculation.Expression,
              opts: [expr: trust(expression, stack)]
          }

        {:error, _} ->
          calculation
      end
    else
      calculation
    end
  end

  defp authorize_calculation?(calculation) do
    Map.get(calculation, :authorize?, false) == true &&
      Map.get(calculation.context, :authorize?) == true
  end

  @doc """
  Filters each relationship in the path of a related `exists` by its destination's read policies.

  Only the last relationship can be filtered in the body of the `exists`, so a path with more than
  one relationship is split into nested `exists`, one per relationship.
  """
  def authorize_exists_path(%Exists{path: path, expr: expr} = exists, resource, path_filters) do
    relationships =
      resource
      |> expand_path_with_through(path)
      |> Enum.map(&elem(&1, 0))

    expr = shift_parents(expr, length(relationships) - 1)

    relationships
    |> Enum.reverse()
    |> Enum.reduce(expr, fn relationship, expr ->
      expr =
        case relationship_filter(relationship, path_filters) do
          nil -> expr
          condition -> BooleanExpression.optimized_new(:and, condition, expr)
        end

      %{exists | path: [relationship.name], at_path: [], expr: expr}
    end)
    |> then(&%{&1 | at_path: exists.at_path})
  end

  @doc """
  The condition for the actor being able to read each related record along a relationship path.
  """
  def relationship_condition(resource, path, path_filters) do
    resource
    |> expand_path_with_through(path)
    |> Enum.reduce(true, fn {relationship, filter_path}, condition ->
      case relationship_filter(relationship, path_filters) do
        nil ->
          condition

        filter ->
          BooleanExpression.optimized_new(
            :and,
            condition,
            Ash.Filter.move_to_relationship_path(filter, filter_path)
          )
      end
    end)
  end

  @doc """
  Expands relationships with `through` into the relationships they go through, along with the
  path to each one.
  """
  def expand_path_with_through(resource, path) do
    expand_path_with_through(resource, path, [], [])
  end

  defp expand_path_with_through(_resource, [], _current_path, acc), do: Enum.reverse(acc)

  defp expand_path_with_through(resource, [name | rest], current_path, acc) do
    relationship = Ash.Resource.Info.relationship(resource, name)

    case Map.get(relationship, :through) do
      through when is_list(through) ->
        nested = expand_path_with_through(resource, through, current_path, [])
        expanded_path = current_path ++ through
        {last_relationship, _} = List.last(nested)

        expand_path_with_through(
          last_relationship.destination,
          rest,
          expanded_path,
          Enum.reverse(nested) ++ acc
        )

      _ ->
        filter_path = current_path ++ [name]
        new_acc = [{relationship, filter_path} | acc]
        expand_path_with_through(relationship.destination, rest, filter_path, new_acc)
    end
  end

  defp relationship_filter(relationship, path_filters) do
    read_action =
      relationship.read_action ||
        Ash.Resource.Info.primary_action!(relationship.destination, :read).name

    case Map.get(path_filters, {relationship.source, relationship.name, read_action}) do
      nil -> nil
      %Ash.Filter{expression: expression} -> expression
    end
  end

  # Nesting the body of an `exists` in more `exists` puts it further away from the resources
  # that `parent/1` refers to.
  defp shift_parents(expr, 0), do: expr
  defp shift_parents(expr, count), do: shift_parents(expr, 0, count)

  defp shift_parents(expr, depth, count) do
    Ash.Filter.map(expr, fn
      %Parent{} = parent when depth == 0 ->
        {:halt, Enum.reduce(1..count, parent, fn _, parent -> %Parent{expr: parent} end)}

      %Parent{expr: expr} = parent ->
        {:halt, %{parent | expr: shift_parents(expr, depth - 1, count)}}

      %Exists{expr: expr} = exists ->
        {:halt, %{exists | expr: shift_parents(expr, depth + 1, count)}}

      %Ref{
        attribute:
          %Ash.Query.Calculation{module: Ash.Resource.Calculation.Expression, opts: calc_opts} =
              calculation
      } = ref ->
        expr = shift_parents(calc_opts[:expr], depth, count)
        {:halt, %{ref | attribute: %{calculation | opts: Keyword.put(calc_opts, :expr, expr)}}}

      other ->
        other
    end)
  end

  # The stack holds `{resource, read_action}` for the resource that the expression is relative
  # to, followed by the resources that `parent/1` refers to. Nested calculations are authorized
  # according to their own `authorize?` option when they are expanded, so we don't look inside of
  # them.
  defp requirements(expression, stack) do
    Ash.Filter.flat_map(expression, fn
      %Ref{} = ref ->
        ref_requirements(ref, stack)

      %Exists{} = exists ->
        exists_requirements(exists, stack)

      %Parent{expr: expr} ->
        case stack do
          [_ | [_ | _] = parents] -> requirements(expr, parents)
          _ -> []
        end

      _ ->
        []
    end)
  end

  defp ref_requirements(ref, [{resource, action} | _]) do
    {destination, destination_action} = destination(resource, action, ref.relationship_path)

    field_requirements =
      case field_name(destination, ref) do
        nil -> []
        field -> [{:field, destination, destination_action, field}]
      end

    relationship_requirements(resource, ref.relationship_path) ++ field_requirements
  end

  defp exists_requirements(
         %Exists{related?: false, resource: destination, expr: expr} = exists,
         stack
       ) do
    [
      {:unrelated_exists, exists}
      | requirements(expr, [{destination, primary_read(destination)} | stack])
    ]
  end

  defp exists_requirements(
         %Exists{at_path: at_path, path: path, expr: expr},
         [{resource, action} | _] = stack
       ) do
    {destination, destination_action} = destination(resource, action, at_path ++ path)

    relationship_requirements(resource, at_path ++ path) ++
      requirements(expr, [{destination, destination_action} | stack])
  end

  defp relationship_requirements(resource, path) do
    resource
    |> expand_path_with_through(path)
    |> Enum.map(fn {relationship, _filter_path} -> {:relationship, relationship} end)
  end

  defp field_conditions(field_items, domain, context) do
    field_items
    |> Enum.group_by(
      fn {:field, resource, action, _field} -> {resource, action} end,
      fn {:field, _resource, _action, field} -> field end
    )
    |> Enum.reject(fn {{resource, _action}, _fields} ->
      Ash.Resource.Info.authorizers(resource) == []
    end)
    |> Enum.reduce(%{}, fn {{resource, action}, fields}, conditions ->
      case Ash.Can.evaluate_field_policies(
             {resource, Ash.Resource.Info.action(resource, action)},
             Ash.Resource.Info.domain(resource) || domain,
             context.actor,
             fields,
             tenant: context.tenant,
             run_queries?: false
           ) do
        {:ok, results} ->
          Enum.reduce(results, conditions, fn {field, result}, conditions ->
            Map.put(conditions, {resource, action, field}, field_condition(result))
          end)

        {:error, error} ->
          raise Ash.Error.to_ash_error(error)
      end
    end)
  end

  defp field_condition({:filter, %Ash.Filter{expression: expression}}), do: expression
  defp field_condition({:filter, expression}), do: expression
  defp field_condition(result), do: result

  defp rewrite(expression, stack, path_filters, field_conditions) do
    Ash.Filter.map(expression, fn
      %Ref{} = ref ->
        {:halt, rewrite_ref(ref, stack, path_filters, field_conditions)}

      %Exists{} = exists ->
        {:halt, rewrite_exists(exists, stack, path_filters, field_conditions)}

      %Parent{expr: expr} = parent ->
        case stack do
          [_ | [_ | _] = parents] ->
            {:halt, %{parent | expr: rewrite(expr, parents, path_filters, field_conditions)}}

          _ ->
            {:halt, parent}
        end

      other ->
        other
    end)
  end

  defp rewrite_ref(ref, [{resource, action} | _], path_filters, field_conditions) do
    {destination, destination_action} = destination(resource, action, ref.relationship_path)

    field_condition =
      case field_name(destination, ref) do
        nil ->
          true

        field ->
          field_conditions
          |> Map.get({destination, destination_action, field}, true)
          |> Ash.Filter.move_to_relationship_path(ref.relationship_path)
      end

    resource
    |> relationship_condition(ref.relationship_path, path_filters)
    |> then(&BooleanExpression.optimized_new(:and, &1, field_condition))
    |> nil_unless(ref)
  end

  defp rewrite_exists(
         %Exists{related?: false, resource: destination, expr: expr} = exists,
         stack,
         path_filters,
         field_conditions
       ) do
    read_action = primary_read(destination)

    expr =
      rewrite(expr, [{destination, read_action} | stack], path_filters, field_conditions)

    case Map.get(path_filters, {:unrelated_exists, destination, read_action}) do
      %Ash.Filter{expression: condition} ->
        %{exists | expr: BooleanExpression.optimized_new(:and, expr, condition)}

      nil ->
        %{exists | expr: expr}
    end
  end

  defp rewrite_exists(
         %Exists{at_path: at_path, path: path, expr: expr} = exists,
         [{resource, action} | _] = stack,
         path_filters,
         field_conditions
       ) do
    {at_resource, _} = destination(resource, action, at_path)
    {destination, destination_action} = destination(resource, action, at_path ++ path)

    expr =
      rewrite(expr, [{destination, destination_action} | stack], path_filters, field_conditions)

    exists = authorize_exists_path(%{exists | expr: expr}, at_resource, path_filters)

    resource
    |> relationship_condition(at_path, path_filters)
    |> nil_unless(exists)
  end

  defp nil_unless(true, value), do: value

  defp nil_unless(condition, value) do
    {:ok, expr} = Ash.Query.Function.If.new([condition, value, nil])
    expr
  end

  defp destination(resource, action, []), do: {resource, action}

  defp destination(resource, _action, path) do
    {relationship, _} = resource |> expand_path_with_through(path) |> List.last()

    {relationship.destination, relationship.read_action || primary_read(relationship.destination)}
  end

  defp primary_read(resource) do
    case Ash.Resource.Info.primary_action(resource, :read) do
      nil -> nil
      action -> action.name
    end
  end

  defp field_name(resource, ref) do
    name =
      case ref.attribute do
        %Ash.Query.Calculation{calc_name: name} -> name
        %Ash.Query.Aggregate{agg_name: name} -> name
        %{name: name} -> name
        name -> name
      end

    if is_atom(name) && !is_nil(name) && Ash.Resource.Info.field(resource, name) do
      name
    end
  end
end
