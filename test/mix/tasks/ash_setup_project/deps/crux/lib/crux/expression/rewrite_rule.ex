# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule do
  @moduledoc """
  Behaviour for defining expression rewrite rules.

  A rewrite rule defines how to transform boolean expressions during traversal.
  Rules can be composed together or applied individually depending on their
  exclusivity requirements.
  """

  alias Crux.Expression

  @type t() :: module()
  @type rule_options() :: [
          exclusive?: boolean(),
          needs_reapplication?: boolean(),
          type: :prewalk | :postwalk
        ]
  @type acc_id(variable, acc) ::
          t() | Expression.walker(variable, acc)
  @type acc_map(variable, acc) :: %{acc_id(variable, acc) => acc}
  @type rule_spec(variable, acc) ::
          t()
          | Expression.walker_stateless(variable)
          | {Expression.walker_stateful(variable, acc), acc}
          | {Expression.walker_stateful(variable, acc), acc, rule_options()}

  @doc """
  Returns whether this rule is exclusive.

  An exclusive rule cannot be composed with other rules and must be applied
  in isolation. Non-exclusive rules can be composed together for efficiency.
  """
  @callback exclusive?() :: boolean()

  @doc """
  Returns the traversal type for this rule.

  Must return either `:prewalk` or `:postwalk` to indicate when during
  tree traversal this rule should be applied.
  """
  @callback type() :: :prewalk | :postwalk

  @doc """
  Transforms an expression with an accumulator.

  This callback is optional - if not implemented, the default implementation
  will call `walk/1` and return `{result, acc}`.
  """
  @callback walk_stateful(Expression.t(variable), acc) :: {Expression.t(variable), acc}
            when variable: term(), acc: term()

  @doc """
  Transforms an expression without an accumulator.

  This is the core transformation logic for the rule.
  """
  @callback walk(Expression.t(variable)) :: Expression.t(variable) when variable: term()

  @doc """
  Returns whether this rule may need reapplication.

  Rules that return `true` may be applied multiple times until a fixpoint
  is reached (no further changes occur). Rules that return `false` are
  applied only once.
  """
  @callback needs_reapplication?() :: boolean()

  @optional_callbacks [walk: 1]

  @doc """
  Applies a list of rewrite rules to an expression.

  Rules are processed in chunks based on their exclusivity and type while
  preserving the order they appear in the list. Adjacent rules with the same
  exclusivity and type are grouped together for efficiency.

  ## Rule Processing

  - Rules are chunked by adjacency of exclusivity and type
  - Exclusive rules always get their own chunk
  - Each chunk is applied sequentially to the result of the previous chunk
  - If any rule in a chunk has `needs_reapplication?() == true`, the chunk
    is reapplied until no changes occur (fixpoint)

  ## Examples

      iex> import Crux.Expression, only: [b: 1]
      ...> expr = b(not not :a)
      ...>
      ...> {result, _acc_map} =
      ...>   RewriteRule.apply(expr, [
      ...>     Crux.Expression.RewriteRule.NegationLaw
      ...>   ])
      ...>
      ...> result
      :a

  """
  @spec apply(Expression.t(variable), [rule_spec(variable, acc)]) ::
          {Expression.t(variable), acc_map(variable, acc)}
        when variable: term(), acc: term()
  def apply(expression, rules) do
    acc_map = initialize_accumulator_map(rules)

    rules
    |> chunk_rules()
    |> Enum.reduce({expression, acc_map}, &apply_chunk_with_reapplication/2)
  end

  @doc false
  @spec default_walk_stateful(module(), Expression.t(variable), acc) ::
          {Expression.t(variable), acc}
        when variable: term(), acc: term()
  def default_walk_stateful(module, expression, acc) do
    {module.walk(expression), acc}
  end

  @spec initialize_accumulator_map([rule_spec(variable, acc)]) :: acc_map(variable, acc)
        when variable: term(), acc: term()
  defp initialize_accumulator_map(rules) do
    Enum.reduce(rules, %{}, fn rule_spec, acc_map ->
      {identifier, initial_acc} = get_rule_identifier_and_initial_acc(rule_spec)
      Map.put_new(acc_map, identifier, initial_acc)
    end)
  end

  @spec get_rule_identifier_and_initial_acc(rule_spec(variable, acc)) ::
          {acc_id(variable, acc), acc}
        when variable: term(), acc: term()
  defp get_rule_identifier_and_initial_acc(module) when is_atom(module) do
    {module, nil}
  end

  defp get_rule_identifier_and_initial_acc(fun) when is_function(fun) do
    {fun, nil}
  end

  defp get_rule_identifier_and_initial_acc({fun, initial_acc}) when is_function(fun) do
    {fun, initial_acc}
  end

  defp get_rule_identifier_and_initial_acc({fun, initial_acc, _options}) when is_function(fun) do
    {fun, initial_acc}
  end

  @spec get_rule_properties(rule_spec(variable, acc)) :: %{
          exclusive?: boolean(),
          type: :prewalk | :postwalk,
          needs_reapplication?: boolean()
        }
        when variable: term(), acc: term()
  defp get_rule_properties(module) when is_atom(module) do
    %{
      exclusive?: module.exclusive?(),
      type: module.type(),
      needs_reapplication?: module.needs_reapplication?()
    }
  end

  defp get_rule_properties(fun) when is_function(fun) do
    %{
      exclusive?: false,
      type: :postwalk,
      needs_reapplication?: false
    }
  end

  defp get_rule_properties({fun, _initial_acc}) when is_function(fun) do
    %{
      exclusive?: false,
      type: :postwalk,
      needs_reapplication?: false
    }
  end

  defp get_rule_properties({fun, _initial_acc, options}) when is_function(fun) do
    %{
      exclusive?: Keyword.get(options, :exclusive?, false),
      type: Keyword.get(options, :type, :postwalk),
      needs_reapplication?: Keyword.get(options, :needs_reapplication?, false)
    }
  end

  @spec get_rule_identifier(rule_spec(variable, acc)) :: acc_id(variable, acc)
        when variable: term(), acc: term()
  defp get_rule_identifier(module) when is_atom(module), do: module
  defp get_rule_identifier(fun) when is_function(fun), do: fun
  defp get_rule_identifier({fun, _initial_acc}) when is_function(fun), do: fun
  defp get_rule_identifier({fun, _initial_acc, _options}) when is_function(fun), do: fun

  @spec create_walker_function(rule_spec(variable, acc), acc_id(variable, acc)) ::
          (Expression.t(variable),
           acc_map(
             variable,
             acc
           ) ->
             {Expression.t(variable),
              acc_map(
                variable,
                acc
              )})
        when variable: term(), acc: term()
  defp create_walker_function(module, identifier) when is_atom(module) do
    fn expr, acc_map ->
      current_acc = Map.get(acc_map, identifier)
      {new_expr, new_acc} = module.walk_stateful(expr, current_acc)
      {new_expr, Map.put(acc_map, identifier, new_acc)}
    end
  end

  defp create_walker_function(fun, _identifier) when is_function(fun) do
    fn expr, acc_map ->
      # Function with no accumulator
      new_expr = fun.(expr)
      {new_expr, acc_map}
    end
  end

  defp create_walker_function({fun, _initial_acc}, identifier) when is_function(fun) do
    fn expr, acc_map ->
      current_acc = Map.get(acc_map, identifier)
      {new_expr, new_acc} = fun.(expr, current_acc)
      {new_expr, Map.put(acc_map, identifier, new_acc)}
    end
  end

  defp create_walker_function({fun, _initial_acc, _options}, identifier) when is_function(fun) do
    fn expr, acc_map ->
      current_acc = Map.get(acc_map, identifier)
      {new_expr, new_acc} = fun.(expr, current_acc)
      {new_expr, Map.put(acc_map, identifier, new_acc)}
    end
  end

  @spec chunk_rules([rule_spec(variable, acc)]) :: [[rule_spec(variable, acc)]]
        when variable: term(), acc: term()
  defp chunk_rules(rules) do
    rules
    |> Enum.reduce([], fn
      rule, [] ->
        [[rule]]

      rule, [current_chunk | rest_chunks] = chunks ->
        if can_chunk_with?(rule, hd(current_chunk)) do
          [[rule | current_chunk] | rest_chunks]
        else
          [[rule] | chunks]
        end
    end)
    |> Enum.map(&Enum.reverse/1)
    |> Enum.reverse()
  end

  @spec can_chunk_with?(rule_spec(variable, acc), rule_spec(variable, acc)) :: boolean()
        when variable: term(), acc: term()
  defp can_chunk_with?(rule1, rule2) do
    props1 = get_rule_properties(rule1)
    props2 = get_rule_properties(rule2)
    !props1.exclusive? and !props2.exclusive? and props1.type == props2.type
  end

  @spec apply_chunk_with_reapplication(
          [rule_spec(variable, acc)],
          {Expression.t(variable), acc_map(variable, acc)}
        ) :: {Expression.t(variable), acc_map(variable, acc)}
        when variable: term(), acc: term()
  defp apply_chunk_with_reapplication(chunk, {expression, acc_map}) do
    {new_expression, new_acc_map} = apply_chunk(chunk, {expression, acc_map})

    if new_expression != expression and chunk_needs_reapplication?(chunk) do
      apply_chunk_with_reapplication(chunk, {new_expression, new_acc_map})
    else
      {new_expression, new_acc_map}
    end
  end

  @spec apply_chunk([rule_spec(variable, acc)], {Expression.t(variable), acc_map(variable, acc)}) ::
          {Expression.t(variable), acc_map(variable, acc)}
        when variable: term(), acc: term()
  defp apply_chunk([rule], {expression, acc_map}) do
    # Single rule - apply directly
    apply_single_rule(rule, {expression, acc_map})
  end

  defp apply_chunk(rules, {expression, acc_map}) do
    # Multiple rules - compose them
    walker_specs = Enum.map(rules, &rule_to_walker_spec/1)
    composed = compose_walker(walker_specs)
    type = get_rule_properties(hd(rules)).type

    case type do
      :prewalk -> apply_walker(expression, acc_map, composed, &Expression.prewalk/3)
      :postwalk -> apply_walker(expression, acc_map, composed, &Expression.postwalk/3)
    end
  end

  @spec apply_single_rule(
          rule_spec(variable, acc),
          {Expression.t(variable), acc_map(variable, acc)}
        ) ::
          {Expression.t(variable), acc_map(variable, acc)}
        when variable: term(), acc: term()
  defp apply_single_rule(rule, {expression, acc_map}) do
    identifier = get_rule_identifier(rule)
    walker_fun = create_walker_function(rule, identifier)
    type = get_rule_properties(rule).type

    case type do
      :prewalk -> Expression.prewalk(expression, acc_map, walker_fun)
      :postwalk -> Expression.postwalk(expression, acc_map, walker_fun)
    end
  end

  @spec rule_to_walker_spec(rule_spec(variable, acc)) ::
          (Expression.t(variable), acc_map(variable, acc) ->
             {Expression.t(variable), acc_map(variable, acc)})
        when variable: term(), acc: term()
  defp rule_to_walker_spec(rule) do
    identifier = get_rule_identifier(rule)
    create_walker_function(rule, identifier)
  end

  @spec apply_walker(Expression.t(variable), acc_map(variable, acc), walker_fun, walk_fun) ::
          {Expression.t(variable), acc_map(variable, acc)}
        when variable: term(),
             acc: term(),
             walker_fun: (Expression.t(variable), acc_map(variable, acc) ->
                            {Expression.t(variable), acc_map(variable, acc)}),
             walk_fun: (Expression.t(variable), acc_map(variable, acc), walker_fun ->
                          {Expression.t(variable), acc_map(variable, acc)})
  defp apply_walker(expression, acc_map, walker_fun, walk_fun) do
    walk_fun.(expression, acc_map, walker_fun)
  end

  @spec chunk_needs_reapplication?([rule_spec(variable, acc)]) :: boolean()
        when variable: term(), acc: term()
  defp chunk_needs_reapplication?(chunk) do
    Enum.any?(chunk, &get_rule_properties(&1).needs_reapplication?)
  end

  @spec compose_walker([walker_fun]) ::
          (Expression.t(variable), acc_map(variable, acc) ->
             {Expression.t(variable), acc_map(variable, acc)})
        when variable: term(),
             acc: term(),
             walker_fun: (Expression.t(variable), acc_map(variable, acc) ->
                            {Expression.t(variable), acc_map(variable, acc)})
  defp compose_walker(walker_specs) do
    fn expression, acc_map ->
      Enum.reduce(walker_specs, {expression, acc_map}, fn walker_func, {expr, current_acc_map} ->
        walker_func.(expr, current_acc_map)
      end)
    end
  end

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def exclusive?, do: false

      @impl unquote(__MODULE__)
      def type, do: :postwalk

      @impl unquote(__MODULE__)
      def walk_stateful(expression, acc) do
        unquote(__MODULE__).default_walk_stateful(__MODULE__, expression, acc)
      end

      @impl unquote(__MODULE__)
      def needs_reapplication?, do: false

      defoverridable exclusive?: 0, type: 0, walk_stateful: 2, needs_reapplication?: 0
    end
  end
end
