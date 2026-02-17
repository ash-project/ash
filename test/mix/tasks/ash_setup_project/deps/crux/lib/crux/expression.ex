# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Crux.Expression do
  @moduledoc """
  Boolean expression representation and manipulation for SAT solving.

  Provides a DSL for creating boolean expressions with the `b/1` macro, plus functions
  for traversal, evaluation, simplification, and CNF conversion.
  """

  alias Crux.Expression.RewriteRule

  @typedoc """
  Represents an expression.
  """
  @type t(variable) ::
          {:and, t(variable), t(variable)}
          | {:or, t(variable), t(variable)}
          | {:not, t(variable)}
          | true
          | false
          | variable

  defguard is_operation(expr)
           when (is_tuple(expr) and elem(expr, 0) == :and and tuple_size(expr) == 3) or
                  (is_tuple(expr) and elem(expr, 0) == :or and tuple_size(expr) == 3) or
                  (is_tuple(expr) and elem(expr, 0) == :not and tuple_size(expr) == 2)

  defguard is_variable(expr) when not is_operation(expr) and not is_boolean(expr)

  defguard is_literal(expr) when is_variable(expr) or is_boolean(expr)

  @typedoc """
  See `t/1`.
  """
  @type t() :: t(term())

  @typedoc """
  Conjunctive Normal Form (CNF) expression.

  CNF is a conjunction (AND) of clauses, where each clause is a
  disjunction (OR) of literals. A literal is either a variable,
  a negated variable, or a boolean constant.

  Examples:
  - `a` (single literal)
  - `a OR b` (single clause)
  - `(a OR b) AND c` (two clauses)
  - `(a OR NOT b) AND (c OR d)` (two clauses with negation)
  """
  @type cnf(variable) :: cnf_conjunction(variable)

  @typedoc """
  A conjunction (AND) of clauses in CNF.

  Can be either a single clause or multiple clauses joined by AND.
  """
  @type cnf_conjunction(variable) ::
          {:and, cnf_conjunction(variable), cnf_conjunction(variable)}
          | cnf_clause(variable)

  @typedoc """
  A clause in CNF - a disjunction (OR) of literals.

  Can be either a single literal or multiple literals joined by OR.
  """
  @type cnf_clause(variable) ::
          {:or, cnf_clause(variable), cnf_clause(variable)}
          | cnf_literal(variable)

  @typedoc """
  A literal in CNF.

  Can be:
  - A variable
  - A negated variable
  - A boolean constant
  """
  @type cnf_literal(variable) :: variable | {:not, variable} | boolean()

  @typedoc """
  A stateless walker function that operates on expressions.

  Takes an expression and returns a transformed expression.
  """
  @type walker_stateless(variable) :: (t(variable) -> t(variable))

  @typedoc """
  A stateful walker function that operates on expressions with an accumulator.

  Takes an expression and an accumulator, returns a transformed expression
  and updated accumulator.
  """
  @type walker_stateful(variable, acc) :: (t(variable), acc -> {t(variable), acc})

  @typedoc """
  A walker function that can be either stateless or stateful.
  """
  @type walker(variable, acc) :: walker_stateless(variable) | walker_stateful(variable, acc)

  @doc """
  Creates tuples of a boolean statement.

  Supports basic boolean operations (`and`, `or`, `not`) as well as higher-order
  boolean operators for common logical patterns.

  ## Examples

      iex> # Complex expression using basic operators
      ...> b((:a and not :b) or (not :c and :d))
      {:or, {:and, :a, {:not, :b}}, {:and, {:not, :c}, :d}}

      iex> # NAND (Not AND) - "not both"
      ...> b(nand(:a, :b))
      {:not, {:and, :a, :b}}

      iex> # NOR (Not OR) - "neither"
      ...> b(nor(:a, :b))
      {:not, {:or, :a, :b}}

      iex> # XOR (Exclusive OR) - "exactly one"
      ...> b(xor(:a, :b))
      {:and, {:or, :a, :b}, {:not, {:and, :a, :b}}}

      iex> # XNOR (Not Exclusive OR) - "both or neither"
      ...> b(xnor(:a, :b))
      {:not, {:and, {:or, :a, :b}, {:not, {:and, :a, :b}}}}

      iex> # IMPLIES - "if A then B"
      ...> b(implies(:a, :b))
      {:not, {:and, :a, {:not, :b}}}

      iex> # IMPLIED_BY - "A if B"
      ...> b(implied_by(:a, :b))
      {:not, {:and, :b, {:not, :a}}}

  """
  defmacro b(ast) do
    Macro.prewalk(
      ast,
      fn
        {:and, _, [left, right]} ->
          quote do
            {:and, unquote(left), unquote(right)}
          end

        {:or, _, [left, right]} ->
          quote do
            {:or, unquote(left), unquote(right)}
          end

        {:not, _, [value]} ->
          quote do
            {:not, unquote(value)}
          end

        {:nand, _, [left, right]} ->
          quote do
            {:not, {:and, unquote(left), unquote(right)}}
          end

        {:nor, _, [left, right]} ->
          quote do
            {:not, {:or, unquote(left), unquote(right)}}
          end

        {:xor, _, [left, right]} ->
          quote do
            {:and, {:or, unquote(left), unquote(right)},
             {:not, {:and, unquote(left), unquote(right)}}}
          end

        {:xnor, _, [left, right]} ->
          quote do
            {:not,
             {:and, {:or, unquote(left), unquote(right)},
              {:not, {:and, unquote(left), unquote(right)}}}}
          end

        {:implies, _, [left, right]} ->
          quote do
            {:not, {:and, unquote(left), {:not, unquote(right)}}}
          end

        {:implied_by, _, [left, right]} ->
          quote do
            {:not, {:and, unquote(right), {:not, unquote(left)}}}
          end

        other ->
          other
      end
    )
  end

  @doc """
  Converts an expression to an AST.

  You can pass a callback to convert variables to AST nodes.
  By default, it uses `Macro.escape/1`.

  ## Examples

      iex> {:and, _meta, [:a, :b]} = to_ast(b(:a and :b))

      # You can customize how variables are represented:
      iex> {:and, _, [{:a, _, []}, {:b, _, []}]} =
      ...>   to_ast(b(:a and :b), fn var -> quote do: unquote(var)() end)

  """
  @spec to_ast(expression :: t(variable), variable_ast_callback :: (variable -> Macro.t())) ::
          Macro.t()
        when variable: term()
  def to_ast(expression, variable_ast_callback \\ &Macro.escape/1)
  def to_ast(true, _), do: true
  def to_ast(false, _), do: false

  def to_ast(b(left and right), callback) do
    quote do
      unquote(to_ast(left, callback)) and unquote(to_ast(right, callback))
    end
  end

  def to_ast(b(left or right), callback) do
    quote do
      unquote(to_ast(left, callback)) or unquote(to_ast(right, callback))
    end
  end

  def to_ast(b(not value), callback) do
    quote do
      not unquote(to_ast(value, callback))
    end
  end

  def to_ast(variable, callback) do
    callback.(variable)
  end

  @doc """
  Converts an expression to an `Inspect.Algebra` document.

  See `Code.quoted_to_algebra/2` for formatting options.

  ## Examples

      iex> b(:a and not :b)
      ...> |> to_algebra()
      ...> |> Inspect.Algebra.format(80)
      ...> |> IO.iodata_to_binary()
      ":a and not :b"

      # You can customize how variables are represented:
      iex> b(:a and not :b)
      ...> |> to_algebra([], fn var -> quote do: unquote(var)() end)
      ...> |> Inspect.Algebra.format(80)
      ...> |> IO.iodata_to_binary()
      "a() and not b()"

  """
  @spec to_algebra(
          expression :: t(variable),
          algebra_opts :: keyword(),
          variable_ast_callback :: (variable -> Macro.t())
        ) :: Inspect.Algebra.t()
        when variable: term()
  def to_algebra(expression, algebra_opts \\ [], variable_ast_callback \\ &Macro.escape/1) do
    expression
    |> to_ast(variable_ast_callback)
    |> Code.quoted_to_algebra(algebra_opts)
  end

  @doc """
  Converts an expression to a string.

  For more control over formatting, use `to_algebra/3` and `Inspect.Algebra.format/2`.

  ## Examples

      iex> #{__MODULE__}.to_string(b(:a and not :b))
      ":a and not :b"

      # You can customize how variables are represented:
      iex> #{__MODULE__}.to_string(b(:a and not :b), fn var -> quote do: unquote(var)() end)
      "a() and not b()"

  """
  @spec to_string(expression :: t(variable), variable_ast_callback :: (variable -> Macro.t())) ::
          String.t()
        when variable: term()
  def to_string(expression, variable_ast_callback \\ &Macro.escape/1) do
    expression
    |> to_ast(variable_ast_callback)
    |> Macro.to_string()
  end

  @doc """
  Performs a depth-first, pre-order traversal of the expression.

  ## Examples

      iex> prewalk(b(:a and :b), fn
      ...>   {:and, left, right} -> {:or, left, right}
      ...>   other -> other
      ...> end)
      {:or, :a, :b}

      iex> prewalk(b(:a and (:b or :c)), fn
      ...>   {:and, left, right} -> {:or, left, right}
      ...>   {:or, left, right} -> {:and, left, right}
      ...>   other -> other
      ...> end)
      {:or, :a, {:and, :b, :c}}

  """
  @spec prewalk(expression :: t(variable), fun :: walker_stateless(variable)) :: t(variable)
        when variable: term()
  def prewalk(expression, fun) do
    {result, _} = prewalk(expression, nil, fn expr, nil -> {fun.(expr), nil} end)
    result
  end

  @doc """
  Performs a depth-first, pre-order traversal of the expression using an accumulator.

  ## Examples

      iex> {_result, acc} =
      ...>   prewalk(b(:a and :b), [], fn
      ...>     {:and, _, _} = expr, acc -> {expr, [:and | acc]}
      ...>     other, acc -> {other, acc}
      ...>   end)
      ...>
      ...> acc
      [:and]

      iex> {_result, count} =
      ...>   prewalk(b(:a and (:b or :c)), 0, fn
      ...>     {:and, _, _} = expr, acc -> {expr, acc + 1}
      ...>     {:or, _, _} = expr, acc -> {expr, acc + 1}
      ...>     other, acc -> {other, acc}
      ...>   end)
      ...>
      ...> count
      2

  """
  @spec prewalk(
          expression :: t(variable),
          acc,
          fun :: walker_stateful(variable, acc)
        ) :: {t(variable), acc}
        when variable: term(), acc: term()
  def prewalk(expression, acc, fun) do
    {expression, acc} = fun.(expression, acc)

    case expression do
      b(left and right) ->
        {left, acc} = prewalk(left, acc, fun)
        {right, acc} = prewalk(right, acc, fun)
        {b(left and right), acc}

      b(left or right) ->
        {left, acc} = prewalk(left, acc, fun)
        {right, acc} = prewalk(right, acc, fun)
        {b(left or right), acc}

      b(not value) ->
        {value, acc} = prewalk(value, acc, fun)
        {b(not value), acc}

      other ->
        {other, acc}
    end
  end

  @doc """
  Performs a depth-first, post-order traversal of the expression.

  ## Examples

      iex> postwalk(b(:a and :b), fn
      ...>   {:and, left, right} -> {:or, left, right}
      ...>   other -> other
      ...> end)
      {:or, :a, :b}

      iex> postwalk(b(:a and (:b or :c)), fn
      ...>   {:and, left, right} -> {:or, left, right}
      ...>   {:or, left, right} -> {:and, left, right}
      ...>   other -> other
      ...> end)
      {:or, :a, {:and, :b, :c}}

  """
  @spec postwalk(expression :: t(variable), fun :: walker_stateless(variable)) :: t(variable)
        when variable: term()
  def postwalk(expression, fun) do
    {result, _} = postwalk(expression, nil, fn expr, nil -> {fun.(expr), nil} end)
    result
  end

  @doc """
  Performs a depth-first, post-order traversal of the expression using an accumulator.

  ## Examples

      iex> {_result, acc} =
      ...>   postwalk(b(:a and :b), [], fn
      ...>     {:and, _, _} = expr, acc -> {expr, [:and | acc]}
      ...>     other, acc -> {other, acc}
      ...>   end)
      ...>
      ...> acc
      [:and]

      iex> {_result, count} =
      ...>   postwalk(b(:a and (:b or :c)), 0, fn
      ...>     {:and, _, _} = expr, acc -> {expr, acc + 1}
      ...>     {:or, _, _} = expr, acc -> {expr, acc + 1}
      ...>     other, acc -> {other, acc}
      ...>   end)
      ...>
      ...> count
      2

  """
  @spec postwalk(
          expression :: t(variable),
          acc,
          fun :: walker_stateful(variable, acc)
        ) :: {t(variable), acc}
        when variable: term(), acc: term()
  def postwalk(expression, acc, fun)

  def postwalk(b(left and right), acc, fun) do
    {left, acc} = postwalk(left, acc, fun)
    {right, acc} = postwalk(right, acc, fun)
    fun.(b(left and right), acc)
  end

  def postwalk(b(left or right), acc, fun) do
    {left, acc} = postwalk(left, acc, fun)
    {right, acc} = postwalk(right, acc, fun)
    fun.(b(left or right), acc)
  end

  def postwalk(b(not value), acc, fun) do
    {value, acc} = postwalk(value, acc, fun)
    fun.(b(not value), acc)
  end

  def postwalk(expression, acc, fun) do
    fun.(expression, acc)
  end

  @doc """
  Checks if an expression is in Conjunctive Normal Form (CNF).

  CNF is a conjunction (AND) of clauses, where each clause is a
  disjunction (OR) of literals. A literal is either a variable,
  a negated variable, or a boolean constant.

  ## Examples

      iex> in_cnf?(b(:a and :b))
      true

      iex> in_cnf?(b(:a or :b))
      true

      iex> in_cnf?(b((:a or :b) and :c))
      true

      iex> in_cnf?(b(:a or (:b and :c)))
      false

      iex> in_cnf?(b(not (:a and :b)))
      false

      iex> in_cnf?(b(not :a))
      true

  """
  @spec in_cnf?(expression :: t(variable)) :: boolean() when variable: term()
  def in_cnf?(expr) do
    check_cnf_structure(expr, :conjunction)
  end

  @spec check_cnf_structure(expression :: t(variable), level :: :conjunction | :clause | :literal) ::
          boolean()
        when variable: term()
  # Check if expression follows CNF structure
  defp check_cnf_structure(b(left and right), :conjunction) do
    # In a conjunction, both sides can be conjunctions or clauses
    check_cnf_structure(left, :conjunction) and check_cnf_structure(right, :conjunction)
  end

  defp check_cnf_structure(b(left or right), :conjunction) do
    # If we see OR at conjunction level, check it as a clause
    check_cnf_structure(b(left or right), :clause)
  end

  defp check_cnf_structure(expr, :conjunction) when is_literal(expr) do
    # Literals at conjunction level are fine
    true
  end

  defp check_cnf_structure(b(not expr), :conjunction) when is_literal(expr) do
    # Negated literals at conjunction level are fine
    true
  end

  defp check_cnf_structure(b(left or right), :clause) do
    # In a clause, both sides must be literals
    check_cnf_structure(left, :literal) and check_cnf_structure(right, :literal)
  end

  defp check_cnf_structure(expr, :clause) do
    # Single item in clause must be a literal
    check_cnf_structure(expr, :literal)
  end

  defp check_cnf_structure(expr, :literal) when is_literal(expr) do
    # Variables and booleans are literals
    true
  end

  defp check_cnf_structure(b(not expr), :literal) when is_literal(expr) do
    # Negated variables and booleans are literals
    true
  end

  defp check_cnf_structure(b(left or right), :literal) do
    # OR of literals is still a literal (clause)
    check_cnf_structure(left, :literal) and check_cnf_structure(right, :literal)
  end

  defp check_cnf_structure(_, _) do
    # Anything else is not valid CNF
    false
  end

  @doc """
  Converts an expression to conjunctive normal form (CNF).

  Applies the following transformations:
  - Negation Law
  - De Morgan's laws (which may create new double negations)
  - Negation Law again
  - Distributive law
  """
  @spec to_cnf(expression :: t(variable)) :: cnf(variable) when variable: term()
  def to_cnf(expression) do
    {result, _acc_map} =
      RewriteRule.apply(expression, [
        RewriteRule.NegationLaw,
        RewriteRule.DeMorgansLaw,
        RewriteRule.NegationLaw,
        RewriteRule.DistributiveLaw
      ])

    result
  end

  @doc """
  Simplifies a boolean expression using all available simplification laws.

  Applies all boolean laws except De Morgan's and Distributive laws, which are
  reserved for CNF conversion. This provides general-purpose simplification
  that preserves logical equivalence while reducing expression complexity.

  ## Examples

      iex> simplify(b(not not :a and (:a and :a)))
      :a

      iex> simplify(b(true and (:x or false)))
      :x

      iex> simplify(b(:a or (not :a or :b)))
      true

  """
  @spec simplify(expression :: t(variable)) :: t(variable) when variable: term()
  def simplify(expression) do
    {result, _acc_map} =
      RewriteRule.apply(expression, [
        RewriteRule.NegationLaw,
        RewriteRule.IdentityLaw,
        RewriteRule.AnnihilatorLaw,
        RewriteRule.IdempotentLaw,
        RewriteRule.AbsorptionLaw,
        RewriteRule.AssociativityLaw,
        RewriteRule.DistributivityBasedSimplificationLaw,
        RewriteRule.ComplementLaw,
        RewriteRule.TautologyLaw,
        RewriteRule.ConsensusTheorem,
        RewriteRule.UnitResolution
      ])

    result
  end

  @doc """
  Balances a boolean expression by normalizing operand order.

  This function sorts operands in AND and OR expressions to create a canonical
  form that makes expressions easier to compare and reason about.
  """
  @spec balance(expression :: t(variable)) :: t(variable) when variable: term()
  def balance(expression) do
    {result, _acc_map} = RewriteRule.apply(expression, [RewriteRule.CommutativityLaw])
    result
  end

  with {:module, StreamData} <- Code.ensure_compiled(StreamData) do
    @doc """
    Generates random boolean expressions for property-based testing.

    Uses StreamData.tree to create recursive boolean expressions with the provided
    leaf generator for terminal values. Automatically includes booleans alongside
    the provided inner generator.
    """
    @spec generate_expression(StreamData.t(variable)) :: StreamData.t(t(variable))
          when variable: term()
    def generate_expression(inner_generator) do
      [StreamData.boolean(), inner_generator]
      |> StreamData.one_of()
      |> StreamData.tree(fn child_expr ->
        StreamData.frequency([
          {2,
           StreamData.map(StreamData.tuple({child_expr, child_expr}), fn {left, right} ->
             b(left and right)
           end)},
          {2,
           StreamData.map(StreamData.tuple({child_expr, child_expr}), fn {left, right} ->
             b(left or right)
           end)},
          {1,
           StreamData.map(child_expr, fn expr ->
             b(not expr)
           end)}
        ])
      end)
    end
  end

  @doc """
  Evaluates a boolean expression using a variable evaluation callback.

  ## Examples

      iex> run(b(:a and :b), fn
      ...>   :a -> true
      ...>   :b -> false
      ...> end)
      false

      iex> run(b(:a or :b), fn
      ...>   :a -> true
      ...>   :b -> false
      ...> end)
      true

      iex> run(b(not :a), fn :a -> true end)
      false

      iex> run(b(:a and (true or :a)), & &1)
      :a

  """
  @spec run(expression :: t(variable), eval_fn :: (variable -> result)) :: t(result)
        when variable: term(), result: term()
  def run(expression, eval_fn) do
    laws = [
      fn
        var when is_variable(var) -> eval_fn.(var)
        other -> other
      end,
      RewriteRule.IdentityLaw,
      RewriteRule.AnnihilatorLaw,
      RewriteRule.IdempotentLaw,
      RewriteRule.NegationLaw
    ]

    {result, _acc_map} = RewriteRule.apply(expression, laws)
    result
  end

  @doc """
  Expands a boolean expression using a callback function with accumulator.

  Similar to `run/2` but exposes the accumulator and uses a callback for expansion
  rather than just variable evaluation. This allows for more complex transformations
  that need to maintain state across the traversal.

  ## Examples

      iex> expand(b(:a and :b), [], fn
      ...>   var, acc when is_variable(var) -> {var, [var | acc]}
      ...>   other, acc -> {other, acc}
      ...> end)
      {b(:a and :b), [:b, :a]}

      iex> expand(b(true or :a), [], fn
      ...>   var, acc when is_variable(var) -> {var, [var | acc]}
      ...>   other, acc -> {other, acc}
      ...> end)
      {true, []}
      # Note: :a is not visited due to short-circuiting

  """
  @spec expand(
          expression :: t(variable),
          acc,
          callback :: (t(variable), acc -> {t(variable), acc})
        ) ::
          {t(variable), acc}
        when variable: term(), acc: term()
  def expand(expression, acc, callback) do
    expression |> simplify() |> do_expand(acc, callback)
  end

  @doc """
  Expands a boolean expression using a callback function without accumulator.

  Convenience function for when no accumulator state is needed.

  ## Examples

      iex> expand(b(:a and true), fn
      ...>   var when is_variable(var) -> var
      ...>   other -> other
      ...> end)
      :a

  """
  @spec expand(expression :: t(variable), callback :: (t(variable) -> t(variable))) :: t(variable)
        when variable: term()
  def expand(expression, callback) do
    {result, _acc} =
      expression
      |> simplify()
      |> expand(nil, fn expr, nil -> {callback.(expr), nil} end)

    result
  end

  @spec do_expand(
          expression :: t(variable),
          acc,
          callback :: (t(variable), acc -> {t(variable), acc})
        ) ::
          {t(variable), acc}
        when variable: term(), acc: term()
  defp do_expand(expression, acc, callback)

  defp do_expand(b(left or right), acc, callback) do
    left
    |> do_expand(acc, callback)
    |> simplify_after_expansion()
    |> case do
      {true, acc} ->
        {true, acc}

      {false, acc} ->
        do_expand(right, acc, callback)

      {left, acc} ->
        right
        |> do_expand(acc, callback)
        |> simplify_after_expansion()
        |> case do
          {true, acc} ->
            {true, acc}

          {false, acc} ->
            {left, acc}

          {right, acc} ->
            {b(left or right), acc}
        end
    end
    |> simplify_after_expansion()
  end

  defp do_expand(b(left and right), acc, callback) do
    left
    |> do_expand(acc, callback)
    |> simplify_after_expansion()
    |> case do
      {false, acc} ->
        {false, acc}

      {true, acc} ->
        do_expand(right, acc, callback)

      {left, acc} ->
        right
        |> do_expand(acc, callback)
        |> simplify_after_expansion()
        |> case do
          {false, acc} ->
            {false, acc}

          {true, acc} ->
            {left, acc}

          {right, acc} ->
            {b(left and right), acc}
        end
    end
  end

  defp do_expand(b(not expr), acc, callback) do
    expr
    |> do_expand(acc, callback)
    |> simplify_after_expansion()
    |> case do
      {true, acc} ->
        {false, acc}

      {false, acc} ->
        {true, acc}

      {b(not expr), acc} ->
        {expr, acc}

      {expr, acc} ->
        {b(not expr), acc}
    end
  end

  defp do_expand(other, acc, callback), do: callback.(other, acc)

  @spec simplify_after_expansion({expression :: t(variable), acc :: acc}) :: {t(variable), acc}
        when variable: term(), acc: term()
  defp simplify_after_expansion({expression, acc}) do
    {simplify_one_expression(expression), acc}
  end

  @spec simplify_one_expression(expression :: t(variable)) :: t(variable) when variable: term()
  defp simplify_one_expression(expression) do
    expression
    |> RewriteRule.IdentityLaw.walk()
    |> RewriteRule.AnnihilatorLaw.walk()
    |> RewriteRule.IdempotentLaw.walk()
    |> RewriteRule.NegationLaw.walk()
    |> RewriteRule.AbsorptionLaw.walk()
    |> RewriteRule.AssociativityLaw.walk()
    |> RewriteRule.DistributivityBasedSimplificationLaw.walk()
    |> RewriteRule.ComplementLaw.walk()
    |> RewriteRule.TautologyLaw.walk()
    |> RewriteRule.ConsensusTheorem.walk()
    |> RewriteRule.UnitResolution.walk()
  end

  @doc """
  Returns an expression that ensures at most one of the given variables can be true.

  This function creates constraints where any two variables cannot both be true
  simultaneously (also known as "mutually exclusive" constraints). It's useful for
  modeling scenarios like user roles, payment methods, or file states where only
  one option should be active at a time.

  ## Examples

      iex> # User roles - person can have at most one role
      ...> at_most_one([:admin, :user, :guest])
      {:and, {:and, {:not, {:and, :admin, :user}}, {:not, {:and, :admin, :guest}}},
       {:not, {:and, :guest, :user}}}

      iex> # Payment methods - choose at most one option
      ...> at_most_one([:credit_card, :paypal])
      {:not, {:and, :credit_card, :paypal}}

      iex> # Empty list always satisfied
      ...> at_most_one([])
      true

      iex> # Single variable always satisfied
      ...> at_most_one([:single])
      true

  ## Truth Table for [:a, :b, :c]

  | :a | :b | :c | Valid? |
  |----|----|----|--------|
  | F  | F  | F  | ✓      |
  | T  | F  | F  | ✓      |
  | F  | T  | F  | ✓      |
  | F  | F  | T  | ✓      |
  | T  | T  | F  | ✗      |
  | T  | F  | T  | ✗      |
  | F  | T  | T  | ✗      |
  | T  | T  | T  | ✗      |

  """
  @spec at_most_one([variable]) :: t(variable) when variable: term()
  def at_most_one(variables) do
    simplify(
      for a <- variables, b <- variables, a != b and a < b, reduce: true do
        acc -> b(acc and nand(a, b))
      end
    )
  end

  @doc """
  Returns an expression that ensures all variables have the same truth value.

  This function creates constraints where either all variables are true together,
  or all are false together (also known as "mutually inclusive" or "biconditional"
  constraints). It's useful for modeling linked states like database transactions,
  feature flags, or cache coherency where states must be synchronized.

  ## Examples

      iex> # Database transaction - both happen or neither
      ...> all_or_none([:begin_transaction, :commit_transaction])
      {:not,
       {:and, {:or, :begin_transaction, :commit_transaction},
        {:not, {:and, :begin_transaction, :commit_transaction}}}}

      iex> # Feature flags - UI and API dark mode go together
      ...> all_or_none([:dark_mode_ui, :dark_mode_api])
      {:not,
       {:and, {:or, :dark_mode_api, :dark_mode_ui}, {:not, {:and, :dark_mode_api, :dark_mode_ui}}}}

      iex> # Empty list always satisfied
      ...> all_or_none([])
      true

      iex> # Single variable always satisfied
      ...> all_or_none([:single])
      true

  ## Truth Table for [:a, :b, :c]

  | :a | :b | :c | Valid? |
  |----|----|----|--------|
  | F  | F  | F  | ✓      |
  | T  | T  | T  | ✓      |
  | T  | F  | F  | ✗      |
  | F  | T  | F  | ✗      |
  | F  | F  | T  | ✗      |
  | T  | T  | F  | ✗      |
  | T  | F  | T  | ✗      |
  | F  | T  | T  | ✗      |

  """
  @spec all_or_none([variable]) :: t(variable) when variable: term()
  def all_or_none(variables) do
    simplify(
      for a <- variables,
          b <- variables,
          a != b and a < b,
          reduce: true do
        acc -> b(acc and xnor(a, b))
      end
    )
  end

  @doc """
  Returns an expression that ensures exactly one of the given variables is true.

  This function combines `at_most_one/1` with additional constraints ensuring that
  at least one variable must be true (also known as "mutually exclusive and
  collectively exhaustive" constraints). It's useful for modeling radio button
  scenarios like order status, user preferences, or permission levels where exactly
  one option must be selected.

  ## Examples

      iex> # User preference - must pick exactly one theme
      ...> exactly_one([:light_theme, :dark_theme])
      {:and, {:or, :light_theme, :dark_theme}, {:not, {:and, :light_theme, :dark_theme}}}

      iex> # Empty list can never satisfy exactly one
      ...> exactly_one([])
      false

      iex> # Single variable must be true
      ...> exactly_one([:single])
      :single

  ## Truth Table for [:a, :b, :c]

  | :a | :b | :c | Valid? |
  |----|----|----|--------|
  | F  | F  | F  | ✗      |
  | T  | F  | F  | ✓      |
  | F  | T  | F  | ✓      |
  | F  | F  | T  | ✓      |
  | T  | T  | F  | ✗      |
  | T  | F  | T  | ✗      |
  | F  | T  | T  | ✗      |
  | T  | T  | T  | ✗      |

  """
  @spec exactly_one([variable]) :: t(variable) when variable: term()
  def exactly_one(variables) do
    variables
    |> Enum.reduce(false, &b((&2 or &1) and nand(&2, &1)))
    |> simplify()
  end
end
