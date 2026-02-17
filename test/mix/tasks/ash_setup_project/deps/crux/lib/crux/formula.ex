# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Formula do
  @moduledoc """
  A module for representing and manipulating satisfiability formulas in
  Conjunctive Normal Form (CNF).
  """

  import Crux.Expression, only: [b: 1]

  alias Crux.Expression

  @typedoc """
  A satisfiability formula in Conjunctive Normal Form (CNF) along with
  bindings that map the integers used in the CNF back to their original values.
  """
  @type t(variable) :: %__MODULE__{
          cnf: cnf(),
          bindings: bindings(variable),
          reverse_bindings: reverse_bindings(variable)
        }

  @typedoc """
  See `t/1`.
  """
  @type t() :: t(term())

  @typedoc """
  A formula in Conjunctive Normal Form (CNF) is a conjunction of clauses,
  where each `clause()` is a disjunction of `literal()`s.

  All `clause()`s of a CNF formula must be satisfied for the formula to be satisfied.
  """
  @type cnf() :: [clause()]

  @typedoc """
  A clause is a disjunction of `literal()`s.

  A clause is satisfied if at least one of its `literal()`s is satisfied.
  """
  @type clause() :: nonempty_list(literal())

  @typedoc """
  A `literal()` is either an `affirmed_literal()` (a positive integer) or
  a `negated_literal()` (a negative integer).
  """
  @type literal() :: affirmed_literal() | negated_literal()

  @typedoc """
  An `affirmed_literal()` is a positive integer representing a variable that is
  asserted to be true.
  """
  @type affirmed_literal() :: pos_integer()

  @typedoc """
  A `negated_literal()` is a negative integer representing a variable that is
  asserted to be false.
  """
  @type negated_literal() :: neg_integer()

  @typedoc """
  A `binding()` maps a positive integer (the variable) to its original value.
  """
  @type bindings(variable) :: %{pos_integer() => variable}

  @typedoc """
  A reverse binding maps a variable to its positive integer representation.
  This provides O(log n) lookup for variable-to-integer mappings.
  """
  @type reverse_bindings(variable) :: %{variable => pos_integer()}

  @typedoc """
  See `bindings/1`.
  """
  @type bindings() :: bindings(term())

  @enforce_keys [:cnf, :bindings, :reverse_bindings]
  defstruct [:cnf, :bindings, :reverse_bindings]

  @simple_true %{__struct__: __MODULE__, cnf: [], bindings: %{}, reverse_bindings: %{}}
  @simple_false %{
    __struct__: __MODULE__,
    cnf: [[1], [-1]],
    bindings: %{1 => false},
    reverse_bindings: %{false => 1}
  }

  @doc """
  Converts a boolean expression to a SAT formula in Conjunctive Normal Form (CNF).

  ## Examples

      iex> import Crux.Expression
      ...> expression = b(:a and :b)
      ...> Formula.from_expression(expression)
      %Formula{
        cnf: [[1], [2]],
        bindings: %{1 => :a, 2 => :b},
        reverse_bindings: %{a: 1, b: 2}
      }

      iex> expression = b(:x or not :y)
      ...> Formula.from_expression(expression)
      %Formula{
        cnf: [[1, -2]],
        bindings: %{1 => :x, 2 => :y},
        reverse_bindings: %{x: 1, y: 2}
      }

  """
  @spec from_expression(Expression.t(variable)) :: t(variable) when variable: term()
  def from_expression(expression) do
    expression
    |> Expression.balance()
    |> Expression.simplify()
    |> case do
      true ->
        @simple_true

      false ->
        @simple_false

      expression ->
        {bindings, reverse_bindings, expression} =
          expression
          |> Expression.to_cnf()
          |> extract_bindings()

        %__MODULE__{
          cnf: expression |> lift_clauses() |> Enum.uniq(),
          bindings: bindings,
          reverse_bindings: reverse_bindings
        }
    end
  end

  @doc """
  Converts a SAT formula back to a boolean expression.

  ## Examples

      iex> formula = %Formula{
      ...>   cnf: [[1], [2]],
      ...>   bindings: %{1 => :a, 2 => :b},
      ...>   reverse_bindings: %{a: 1, b: 2}
      ...> }
      ...>
      ...> Formula.to_expression(formula)
      b(:a and :b)

      iex> formula = %Formula{
      ...>   cnf: [[1, -2]],
      ...>   bindings: %{1 => :x, 2 => :y},
      ...>   reverse_bindings: %{x: 1, y: 2}
      ...> }
      ...>
      ...> Formula.to_expression(formula)
      b(:x or not :y)

  """
  @spec to_expression(formula :: t(variable)) :: Expression.cnf(variable) when variable: term()
  def to_expression(formula)
  def to_expression(@simple_true), do: true
  def to_expression(@simple_false), do: false

  def to_expression(%__MODULE__{cnf: cnf, bindings: bindings}) do
    cnf
    |> Enum.map(&clause_to_expression(&1, bindings))
    |> Enum.reduce(&b(&2 and &1))
  end

  @doc """
  Formats a CNF formula to PicoSAT DIMACS format.

  Takes a formula struct and returns a string in the DIMACS CNF format
  that can be consumed by SAT solvers like PicoSAT.

  ## Examples

      iex> alias Crux.{Expression, Formula}
      ...> formula = Formula.from_expression(Expression.b(:a and :b))
      ...> Formula.to_picosat(formula)
      "p cnf 2 2\\n1 0\\n2 0"

  """
  @spec to_picosat(t()) :: String.t()
  def to_picosat(%__MODULE__{cnf: clauses, bindings: bindings}) do
    variable_count = map_size(bindings)
    clause_count = length(clauses)

    formatted_input =
      Enum.map_join(clauses, "\n", fn clause ->
        Enum.join(clause, " ") <> " 0"
      end)

    "p cnf #{variable_count} #{clause_count}\n" <> formatted_input
  end

  @doc false
  @spec simple_true() :: t()
  def simple_true, do: @simple_true

  @doc false
  @spec simple_false() :: t()
  def simple_false, do: @simple_false

  @spec clause_to_expression(clause(), bindings(variable)) :: Expression.t(variable)
        when variable: term()
  defp clause_to_expression(clause, bindings) do
    clause
    |> Enum.map(&literal_to_expression(&1, bindings))
    |> Enum.reduce(&b(&2 or &1))
  end

  @spec literal_to_expression(literal(), bindings(variable)) :: Expression.t(variable)
        when variable: term()
  defp literal_to_expression(literal, bindings)

  defp literal_to_expression(literal, bindings) when literal > 0,
    do: Map.fetch!(bindings, literal)

  defp literal_to_expression(literal, bindings) when literal < 0,
    do: b(not Map.fetch!(bindings, -literal))

  @spec extract_bindings(expression :: Expression.t(variable)) ::
          {bindings(variable), reverse_bindings(variable), Expression.t(literal())}
        when variable: term()
  defp extract_bindings(expression) do
    {expression, bindings} = Expression.postwalk(expression, %{current: 1}, &bind_variable/2)

    reverse_bindings = Map.delete(bindings, :current)
    forward_bindings = Map.new(reverse_bindings, &{elem(&1, 1), elem(&1, 0)})

    {forward_bindings, reverse_bindings, expression}
  end

  @spec bind_variable(Expression.t(variable), bindings_map) ::
          {pos_integer() | Expression.t(variable), bindings_map}
        when variable: term(),
             bindings_map: %{required(:current) => pos_integer(), variable => pos_integer()}
  defp bind_variable(expr, bindings)

  defp bind_variable(value, %{current: current} = bindings) when Expression.is_variable(value) do
    case Map.fetch(bindings, value) do
      :error ->
        {current, bindings |> Map.update!(:current, &(&1 + 1)) |> Map.put(value, current)}

      {:ok, binding} ->
        {binding, bindings}
    end
  end

  defp bind_variable(expr, bindings) when not is_boolean(expr) do
    {expr, bindings}
  end

  @spec lift_clauses(expression :: Expression.cnf_conjunction(literal())) :: cnf()
  defp lift_clauses(expression)
  defp lift_clauses(b(left and right)), do: lift_clauses(left) ++ lift_clauses(right)

  defp lift_clauses(b(left or right)),
    do: [flatten_or_literals(left) ++ flatten_or_literals(right)]

  defp lift_clauses(b(not value)), do: [[-value]]
  defp lift_clauses(value), do: [[value]]

  @spec flatten_or_literals(expression :: Expression.cnf_clause(literal())) :: [literal()]
  defp flatten_or_literals(b(left or right)),
    do: flatten_or_literals(left) ++ flatten_or_literals(right)

  defp flatten_or_literals(b(not value)), do: [-value]
  defp flatten_or_literals(value), do: [value]
end
