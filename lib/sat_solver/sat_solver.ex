# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.SatSolver do
  @moduledoc """
  Tools for working with the satsolver that drives filter subset checking (for authorization)

  This is public as a very low level toolkit for writing authorizers, but you almost certainly
  do not need to look at this module.

  If you are looking for information about how authorization works, see the [policy guide](/documentation/topics/security/policies.md)
  """

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Formula
  require Expression

  @typedoc """
  A binary decision tree for exploring satisfying assignments.

  Each node is either:
  - `{variable, left_tree, right_tree}` where left_tree is the result when variable=false
    and right_tree is the result when variable=true
  - `true` if the formula is satisfiable
  - `false` if the formula is unsatisfiable
  """
  @type tree(variable) ::
          {variable, tree(variable) | boolean(), tree(variable) | boolean()} | boolean()

  @typedoc """
  Options for decision tree and scenario generation functions.

  - `:sorter` - A comparison function for variable ordering
  - `:conflicts?` - A function that returns true if two variables conflict
  - `:implies?` - A function that returns true if the first variable implies the second
  """
  @type opts(variable) :: [
          {:sorter, (variable, variable -> boolean())}
          | {:conflicts?, (variable, variable -> boolean())}
          | {:implies?, (variable, variable -> boolean())}
        ]

  @typep negation_encoding :: %{
           selectors: [Formula.literal()],
           gadget_clauses: [Formula.clause()]
         }

  @doc """
  Solves a SAT formula and returns a satisfying assignment.

  Returns `{:ok, solution}` with a map of variable assignments, or
  `{:error, :unsatisfiable}` if no solution exists.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      iex> SatSolver.solve(formula)
      {:ok, %{a: true, b: true}}

      iex> formula = Formula.from_expression(Expression.b(:a and not :a))
      iex> SatSolver.solve(formula)
      {:error, :unsatisfiable}

  """
  @spec solve(Formula.t(variable)) :: {:ok, %{variable => boolean()}} | {:error, :unsatisfiable}
        when variable: term()
  def solve(%Formula{cnf: cnf, bindings: bindings}) do
    case Ash.SatSolver.Implementation.solve_expression(cnf) do
      {:ok, solution} -> {:ok, unbind_scenario(solution, bindings)}
      {:error, :unsatisfiable} = error -> error
    end
  end

  @doc """
  Returns true if the formula is satisfiable (has at least one solution), false otherwise.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a or :b))
      iex> SatSolver.satisfiable?(formula)
      true

      iex> formula = Formula.from_expression(Expression.b(:a and not :a))
      iex> SatSolver.satisfiable?(formula)
      false

  """
  @spec satisfiable?(Formula.t()) :: boolean()
  def satisfiable?(%Formula{cnf: cnf}) do
    case Ash.SatSolver.Implementation.solve_expression(cnf) do
      {:ok, _solution} -> true
      {:error, :unsatisfiable} -> false
    end
  end

  @doc """
  Builds a binary decision tree exploring all satisfying assignments.

  The tree represents all possible ways to assign boolean values to variables
  such that the formula is satisfied. Variables are processed in the order
  determined by the sorter function.

  ## Parameters

  - `formula` - The SAT formula to explore
  - `opts` - Keyword list of options

  ## Options

  - `:sorter` - A comparison function `(variable, variable -> boolean())` for variable ordering.
    Defaults to `&<=/2`.
  - `:conflicts?` - A function `(variable, variable -> boolean())` that returns true if the two
    variables conflict with each other. Used to detect when variables are mutually exclusive.
    Defaults to `fn _, _ -> false end`.
  - `:implies?` - A function `(variable, variable -> boolean())` that returns true if the first
    variable implies the value of the second variable. Defaults to `fn _, _ -> false end`.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      iex> SatSolver.decision_tree(formula)
      {:a, false, {:b, false, true}}

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      iex> SatSolver.decision_tree(formula, sorter: &>=/2)
      {:b, false, {:a, false, true}}

  """
  @spec decision_tree(Formula.t(variable), opts(variable)) :: tree(variable)
        when variable: term()
  def decision_tree(formula, opts \\ []) do
    # Set default options
    opts =
      Keyword.merge(
        [
          sorter: &<=/2,
          conflicts?: fn _, _ -> false end,
          implies?: fn _, _ -> false end
        ],
        opts
      )

    sorter = opts[:sorter]

    variables =
      formula.bindings
      |> Map.values()
      |> Enum.sort(sorter)

    build_tree_recursive(formula.cnf, formula, variables, [], opts)
  end

  @doc """
  Finds all satisfying assignments for a formula.

  Takes a formula and returns a list of maps, where each map represents
  a complete variable assignment that satisfies the formula.

  ## Parameters

  - `formula` - The SAT formula to explore
  - `opts` - Keyword list of options

  ## Options

  - `:conflicts?` - A function `(variable, variable -> boolean())` that returns true if the two
    variables conflict with each other. Used to filter out impossible scenarios.
    Defaults to `fn _, _ -> false end`.
  - `:implies?` - A function `(variable, variable -> boolean())` that returns true if the first
    variable implies the value of the second variable. Used to minimize scenarios.
    Defaults to `fn _, _ -> false end`.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      iex> SatSolver.satisfying_scenarios(formula)
      [%{a: true, b: true}]

      iex> formula = Formula.from_expression(Expression.b(:a or :b))
      iex> SatSolver.satisfying_scenarios(formula) |> Enum.sort()
      [%{a: true}, %{b: true}]

  """
  @spec satisfying_scenarios(Formula.t(variable), opts(variable)) :: [%{variable => boolean()}]
        when variable: term()
  def satisfying_scenarios(formula, opts \\ []) do
    original_cnf = formula.cnf
    negation_encoding = build_negation_encoding(original_cnf)

    formula
    |> enumerate_dimacs_scenarios(original_cnf, negation_encoding, [])
    |> Enum.uniq()
    |> Enum.map(&validate_assignments(&1, opts))
    |> Enum.flat_map(fn
      {:ok, scenario} -> [Map.new(scenario)]
      {:error, :unsatisfiable} -> []
    end)
  end

  @doc """
  Validates a collection of variable assignments using domain knowledge.

  Takes an enumerable of `{variable, boolean()}` pairs and validates them using
  the provided conflict and implication callbacks. Variables are processed in
  sorted order.

  Returns `{:ok, filtered_assignments}` if all assignments are valid, or
  `{:error, :unsatisfiable}` if any conflicts are detected.

  ## Parameters

  - `assignments` - An enumerable of `{variable, boolean()}` pairs
  - `opts` - Options containing `:sorter`, `:conflicts?`, and `:implies?` callbacks

  ## Examples

      iex> assignments = [a: true, c: true]
      iex> opts = [implies?: fn :a, :c -> true; _, _ -> false end]
      iex> SatSolver.validate_assignments(assignments, opts)
      {:ok, [a: true]}

  """
  @spec validate_assignments(Enumerable.t({variable, boolean()}), opts(variable)) ::
          {:ok, [{variable, boolean()}]} | {:error, :unsatisfiable}
        when variable: term()
  def validate_assignments(assignments, opts \\ []) do
    sorter = Keyword.get(opts, :sorter, &<=/2)

    assignments
    |> Enum.sort_by(&elem(&1, 0), sorter)
    |> Enum.reduce_while({:ok, []}, fn {variable, value} = assignment, {:ok, acc} ->
      case validate_assignment(acc, assignment, opts) do
        :implied -> {:cont, {:ok, acc}}
        :conflict -> {:halt, {:error, :unsatisfiable}}
        :ok -> {:cont, {:ok, [{variable, value} | acc]}}
      end
    end)
    |> case do
      {:ok, filtered_assignments} -> {:ok, Enum.reverse(filtered_assignments)}
      error -> error
    end
  end

  @spec enumerate_dimacs_scenarios(Formula.t(variable), Formula.cnf(), negation_encoding(), [
          %{variable => boolean()}
        ]) :: [%{variable => boolean()}]
        when variable: term()
  defp enumerate_dimacs_scenarios(
         %Formula{} = working_formula,
         original_cnf,
         negation_encoding,
         acc
       ) do
    case Ash.SatSolver.Implementation.solve_expression(working_formula.cnf) do
      {:error, :unsatisfiable} ->
        acc

      {:ok, solver_model} ->
        variable_ids = Map.keys(working_formula.bindings)
        cube = cube_from_model(solver_model, variable_ids)

        minimized_cube = minimize_cube_dimacs(original_cnf, negation_encoding, cube)

        scenario = unbind_scenario(minimized_cube, working_formula.bindings)

        blocked_formula = add_blocking_clause_dimacs(working_formula, minimized_cube)

        enumerate_dimacs_scenarios(blocked_formula, original_cnf, negation_encoding, [
          scenario | acc
        ])
    end
  end

  # Helper functions for DIMACS-based scenario enumeration

  # Converts SAT solver model to a complete assignment cube over known variables.
  # The solver model contains positive literals for true variables and negative for false.
  # We only include variables from the formula bindings to avoid auxiliary SAT variables.
  @spec cube_from_model([Formula.literal()], [Formula.literal()]) :: [Formula.literal()]
  defp cube_from_model(solver_model, variable_ids) do
    Enum.map(variable_ids, fn var_id ->
      cond do
        var_id in solver_model -> var_id
        -var_id in solver_model -> -var_id
        true -> var_id
      end
    end)
  end

  # Removes unnecessary literals from a satisfying assignment cube.
  # Uses implication testing: if the remaining literals still imply the formula,
  # then the removed literal was redundant.
  @spec minimize_cube_dimacs(Formula.cnf(), negation_encoding(), [Formula.literal()]) :: [
          Formula.literal()
        ]
  defp minimize_cube_dimacs(original_cnf, negation_encoding, cube) do
    Enum.reduce(cube, cube, fn literal, current_cube ->
      trial_cube = List.delete(current_cube, literal)

      if implies_formula?(trial_cube, original_cnf, negation_encoding) do
        trial_cube
      else
        current_cube
      end
    end)
  end

  # Tests whether a set of assumptions logically implies the original formula.
  # Uses the encoding: assumptions ⇒ formula iff (assumptions ∧ ¬formula) is unsatisfiable.
  # The negation encoding provides the ¬formula representation.
  @spec implies_formula?([Formula.literal()], Formula.cnf(), negation_encoding()) :: boolean()
  defp implies_formula?(assumptions, original_cnf, %{
         selectors: selector_vars,
         gadget_clauses: encoding_clauses
       }) do
    if original_cnf == [] do
      true
    else
      query_cnf =
        Enum.map(assumptions, &[&1]) ++
          [selector_vars] ++
          encoding_clauses

      case Ash.SatSolver.Implementation.solve_expression(query_cnf) do
        {:error, :unsatisfiable} -> true
        {:ok, _} -> false
      end
    end
  end

  # Builds an encoding for the negation of a CNF formula.
  # Creates selector variables for each clause and gadget clauses that encode:
  # "if selector_i is true, then clause_i is false"
  # Used for implication testing via satisfiability checking.
  @spec build_negation_encoding(Formula.cnf()) :: negation_encoding()
  defp build_negation_encoding(cnf) do
    variables = cnf |> List.flatten() |> Enum.map(&abs/1)
    max_variable_id = Enum.max([0 | variables])
    clause_count = length(cnf)

    first_selector = max_variable_id + 1
    last_selector = max_variable_id + clause_count
    selector_vars = Enum.to_list(first_selector..last_selector//1)

    encoding_clauses =
      Enum.zip(cnf, selector_vars)
      |> Enum.flat_map(fn {clause, selector_var} ->
        Enum.map(clause, fn literal -> [-selector_var, -literal] end)
      end)

    %{selectors: selector_vars, gadget_clauses: encoding_clauses}
  end

  # Adds a blocking clause to prevent rediscovering the same minimal assignment.
  # The blocking clause is the disjunction of negated literals from the minimized cube,
  # which ensures that any superset of this assignment is also blocked.
  @spec add_blocking_clause_dimacs(Formula.t(variable), [Formula.literal()]) ::
          Formula.t(variable)
        when variable: term()
  defp add_blocking_clause_dimacs(%Formula{} = formula, minimized_cube) do
    blocking_clause = Enum.map(minimized_cube, &(-&1))
    %Formula{formula | cnf: [blocking_clause | formula.cnf]}
  end

  @spec unbind_scenario([Formula.literal()], Formula.bindings(variable)) :: %{
          variable => boolean()
        }
        when variable: term()
  defp unbind_scenario(scenario, bindings) do
    Map.new(scenario, fn
      literal when literal > 0 -> {Map.fetch!(bindings, literal), true}
      literal when literal < 0 -> {Map.fetch!(bindings, -literal), false}
    end)
  end

  @spec build_tree_recursive(
          Formula.cnf(),
          Formula.t(variable),
          [variable],
          [{variable, boolean()}],
          opts(variable)
        ) :: tree(variable)
        when variable: term()
  defp build_tree_recursive(cnf, _formula, [], _path, _opts) do
    case Ash.SatSolver.Implementation.solve_expression(cnf) do
      {:ok, _} -> true
      {:error, :unsatisfiable} -> false
    end
  end

  defp build_tree_recursive(
         cnf,
         formula,
         [variable | remaining_variables],
         path,
         opts
       ) do
    true_validation = validate_assignment(path, {variable, true}, opts)
    false_validation = validate_assignment(path, {variable, false}, opts)

    case {false_validation, true_validation} do
      {:implied, _} ->
        build_single_branch(cnf, formula, variable, false, remaining_variables, path, opts)

      {_, :implied} ->
        build_single_branch(cnf, formula, variable, true, remaining_variables, path, opts)

      {:conflict, :conflict} ->
        false

      {:conflict, _} ->
        build_single_branch(cnf, formula, variable, true, remaining_variables, path, opts)

      {_, :conflict} ->
        build_single_branch(cnf, formula, variable, false, remaining_variables, path, opts)

      {:ok, :ok} ->
        left_tree =
          build_single_branch(cnf, formula, variable, false, remaining_variables, path, opts)

        right_tree =
          build_single_branch(cnf, formula, variable, true, remaining_variables, path, opts)

        # Optimization: if both branches are the same, skip this variable
        case {left_tree, right_tree} do
          {same, same} -> same
          _ -> {variable, left_tree, right_tree}
        end
    end
  end

  @spec build_single_branch(
          Formula.cnf(),
          Formula.t(variable),
          variable,
          boolean(),
          [variable],
          [{variable, boolean()}],
          opts(variable)
        ) :: boolean() | {variable, any(), any()}
        when variable: term()
  defp build_single_branch(cnf, formula, variable, value, remaining_variables, path, opts) do
    constrained_cnf = add_constraint_to_cnf(cnf, formula, variable, value)

    case Ash.SatSolver.Implementation.solve_expression(constrained_cnf) do
      {:ok, _} ->
        build_tree_recursive(
          constrained_cnf,
          formula,
          remaining_variables,
          [{variable, value} | path],
          opts
        )

      {:error, :unsatisfiable} ->
        false
    end
  end

  @spec add_constraint_to_cnf(Formula.cnf(), Formula.t(variable), variable, boolean()) ::
          Formula.cnf()
        when variable: term()
  defp add_constraint_to_cnf(cnf, formula, variable, value) do
    integer_var = Map.fetch!(formula.reverse_bindings, variable)
    constraint_clause = if value, do: [integer_var], else: [-integer_var]
    [constraint_clause | cnf]
  end

  # Validates a single variable assignment against existing assignments
  @spec validate_assignment(
          Enumerable.t({variable, boolean()}),
          {variable, boolean()},
          opts(variable)
        ) :: :ok | :conflict | :implied
        when variable: term()
  defp validate_assignment(existing_assignments, {variable, value}, opts) do
    conflicts? = Keyword.get(opts, :conflicts?, fn _, _ -> false end)
    implies? = Keyword.get(opts, :implies?, fn _, _ -> false end)

    # Extract variables that are set to true in existing assignments
    true_vars = for {var, true} <- existing_assignments, do: var

    cond do
      # Forward implication: Check if any existing true variable implies this variable
      value and Enum.any?(true_vars, &implies?.(&1, variable)) ->
        :implied

      # Backward implication conflict: If this variable is false, check if any existing
      # true variable implies this variable (which would be a conflict)
      not value and Enum.any?(true_vars, &implies?.(&1, variable)) ->
        :conflict

      # Direct conflict: Check if this variable (when true) conflicts with existing assignments
      # (check both directions for user convenience)
      value and Enum.any?(true_vars, &(conflicts?.(&1, variable) or conflicts?.(variable, &1))) ->
        :conflict

      true ->
        :ok
    end
  end

  # ============================================================================
  # DEPRECATED FUNCTIONS - TODO: Remove in V4.0.0
  # ============================================================================

  @doc deprecated: "Use Ash.SatSolver.Expression.b/1 instead"
  defmacro b(statement) do
    IO.warn("Deprecated, use Ash.SatSolver.Expression.b/1 instead", __CALLER__)

    Macro.prewalk(
      statement,
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

        other ->
          other
      end
    )
  end

  @doc deprecated: "Use `Ash.Filter.strict_subset/2` instead."
  def strict_filter_subset(filter, candidate) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.Filter.strict_subset/2` instead.", stacktrace)

    Ash.Filter.strict_subset(filter, candidate)
  end

  @doc deprecated: "Use Ash.Expr.to_sat_expression/2 instead"
  def transform(resource, expression) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    IO.warn(
      "Ash.SatSolver.transform/2 is deprecated, use Ash.Expr.to_sat_expression/2 instead",
      stacktrace
    )

    Ash.Expr.to_sat_expression(resource, expression)
  end

  @doc deprecated: """
       Use the following instead:

           Ash.Expr.to_sat_expression/2
           |> Ash.SatSolver.Formula.from_expression()
           |> Ash.SatSolver.solve()
       """
  def transform_and_solve(resource, expression) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    IO.warn(
      """
      Ash.SatSolver.transform_and_solve/2 is deprecated. Use the following instead:

          Ash.Expr.to_sat_expression/2
          |> Ash.SatSolver.Formula.from_expression()
          |> Ash.SatSolver.solve()
      """,
      stacktrace
    )

    resource
    |> Ash.Expr.to_sat_expression(expression)
    |> Ash.SatSolver.Formula.from_expression()
    |> solve()
    |> case do
      {:error, :unsatisfiable} ->
        {:error, :unsatisfiable}

      {:ok, scenario} ->
        # Fake Indexes for old Format since this is a public function
        # Does not make a lot of sense because the indexes without the bindings
        # are not very meaningful...
        indices =
          scenario
          |> Enum.with_index(1)
          |> Enum.map(fn
            {{_variable, true}, index} -> index
            {{_variable, false}, index} -> 0 - index
          end)

        {:ok, indices}
    end
  end

  @doc deprecated: "Use `Ash.Resource.Info.synonymous_relationship_paths?/4` instead."
  def synonymous_relationship_paths?(
        left_resource,
        candidate,
        search,
        right_resource \\ nil
      ) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.Resource.Info.synonymous_relationship_paths?/4` instead.", stacktrace)

    Ash.Resource.Info.synonymous_relationship_paths?(
      left_resource,
      candidate,
      search,
      right_resource
    )
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.at_most_one/1` instead."
  def mutually_exclusive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.at_most_one/1` instead.", stacktrace)

    case Expression.at_most_one(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.exactly_one/1` instead."
  def mutually_exclusive_and_collectively_exhaustive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.exactly_one/1` instead.", stacktrace)

    case Expression.exactly_one(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.b(nand(left, right))` instead."
  def left_excludes_right(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.b(nand(left, right))` instead.", stacktrace)

    Expression.b(nand(left, right))
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.b(nand(left, right))` instead."
  def right_excludes_left(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.b(nand(left, right))` instead.", stacktrace)

    Expression.b(nand(left, right))
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.all_or_none/1` instead."
  def mutually_inclusive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.all_or_none/1` instead.", stacktrace)

    case Expression.all_or_none(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.b(implied_by(left, right))` instead."
  def right_implies_left(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.b(implied_by(left, right))` instead.", stacktrace)

    Expression.b(implied_by(left, right))
  end

  @doc deprecated: "Use `Ash.SatSolver.Expression.b(implies(left, right))` instead."
  def left_implies_right(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.SatSolver.Expression.b(implies(left, right))` instead.", stacktrace)

    Expression.b(implies(left, right))
  end

  @doc deprecated: "Use Ash.SatSolver.Formular.from_expression/1 instead"
  def to_cnf(expression) do
    %Ash.SatSolver.Formula{cnf: cnf, bindings: bindings} =
      Ash.SatSolver.Formula.from_expression(expression)

    current = bindings |> Map.keys() |> Enum.max(fn -> 0 end)

    old_bindings = Map.put(bindings, :current, current + 1)

    temp_bindings =
      1..current//1
      |> Map.new(&{&1, &1})
      |> then(
        &Map.merge(&1, %{
          reverse: &1,
          current: current
        })
      )

    {cnf,
     %{
       temp_bindings: temp_bindings,
       old_bindings: old_bindings
     }}
  end
end
