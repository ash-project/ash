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

  alias Ash.Filter
  alias Ash.Query.{BooleanExpression, Not, Ref}
  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Formula
  require Expression

  @dialyzer {:nowarn_function, overlap?: 2}

  @type boolean_expr() ::
          {:and, boolean_expr, boolean_expr}
          | {:or, boolean_expr, boolean_expr}
          | {:not, boolean_expr}
          | Ash.Expr.t()
  @type boolean_expr(custom) :: boolean_expr() | custom

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

  @doc """
  Creates tuples of a boolean statement.

  i.e `b(1 and 2) #=> {:and, 1, 2}`
  """
  defmacro b(statement) do
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

  @doc "Returns true if the candidate filter returns the same or less data than the filter"
  @spec strict_filter_subset(Ash.Filter.t(), Ash.Filter.t()) :: boolean | :maybe
  def strict_filter_subset(filter, candidate) do
    case {filter, candidate} do
      {%{expression: nil}, %{expression: nil}} ->
        true

      {%{expression: nil}, _candidate_expr} ->
        true

      {_filter_expr, %{expression: nil}} ->
        false

      {filter, candidate} ->
        do_strict_filter_subset(filter, candidate)
    end
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

  @doc "Calls `transform/2` and solves the expression"
  @spec transform_and_solve(Ash.Resource.t(), Ash.Expr.t()) ::
          {:ok, [integer()]} | {:error, :unsatisfiable}
  def transform_and_solve(resource, expression) do
    resource
    |> transform(expression)
    |> to_cnf()
    |> elem(0)
    |> Ash.SatSolver.Implementation.solve_expression()
  end

  @doc false
  def balance({op, left, right}) do
    left = balance(left)
    right = balance(right)
    [left, right] = Enum.sort([left, right])

    {op, left, right}
  end

  def balance({:not, {:not, right}}) do
    balance(right)
  end

  def balance({:not, statement}) do
    {:not, balance(statement)}
  end

  def balance(other), do: other

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

  defp do_strict_filter_subset(filter, candidate) do
    filter =
      Filter.map(filter, fn
        %Ref{} = ref ->
          %{ref | input?: false}

        other ->
          other
      end)

    candidate =
      Filter.map(candidate, fn
        %Ref{} = ref ->
          %{ref | input?: false}

        other ->
          other
      end)

    expr = BooleanExpression.new(:and, filter.expression, candidate.expression)

    case transform_and_solve(
           filter.resource,
           expr
         ) do
      {:error, :unsatisfiable} ->
        false

      {:ok, _scenario} ->
        expr = BooleanExpression.new(:and, Not.new(filter.expression), candidate.expression)

        case transform_and_solve(
               filter.resource,
               expr
             ) do
          {:error, :unsatisfiable} ->
            true

          {:ok, _scenario} ->
            :maybe
        end
    end
  end

  @doc "Returns a statement expressing that the predicates are mutually exclusive."
  @spec mutually_exclusive([Ash.Expr.t()]) :: boolean_expr()
  def mutually_exclusive(predicates, acc \\ [])
  def mutually_exclusive([], acc), do: acc

  def mutually_exclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b(not (predicate and other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  @doc "Returns a statement expressing that the predicates are mutually exclusive and collectively exhaustive."
  @spec mutually_exclusive_and_collectively_exhaustive([Ash.Expr.t()]) :: boolean_expr()
  def mutually_exclusive_and_collectively_exhaustive([]), do: []

  def mutually_exclusive_and_collectively_exhaustive([_]), do: []

  def mutually_exclusive_and_collectively_exhaustive(predicates) do
    mutually_exclusive(predicates) ++
      Enum.flat_map(predicates, fn predicate ->
        other_predicates = Enum.reject(predicates, &(&1 == predicate))

        other_predicates_union =
          Enum.reduce(other_predicates, nil, fn other_predicate, expr ->
            if expr do
              b(expr or other_predicate)
            else
              other_predicate
            end
          end)

        b(
          not (predicate and other_predicates_union) and
            not (not predicate and not other_predicates_union)
        )
      end)
  end

  @doc "Returns `b(not (left and right))`"
  @spec left_excludes_right(Ash.Expr.t(), Ash.Expr.t()) :: boolean_expr()
  def left_excludes_right(left, right) do
    b(not (left and right))
  end

  @doc "Returns `b(not (right and left))`"
  @spec right_excludes_left(Ash.Expr.t(), Ash.Expr.t()) :: boolean_expr()
  def right_excludes_left(left, right) do
    b(not (right and left))
  end

  @doc "Returns a statement expressing that the predicates are mutually inclusive"
  @spec mutually_inclusive([Ash.Expr.t()]) :: boolean_expr()
  def mutually_inclusive(predicates, acc \\ [])
  def mutually_inclusive([], acc), do: acc

  def mutually_inclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b((predicate and other_predicate) or (not predicate and not other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  @doc "Returns `b(not (right and not left))`"
  @spec right_implies_left(Ash.Expr.t(), Ash.Expr.t()) :: boolean_expr()
  def right_implies_left(left, right) do
    b(not (right and not left))
  end

  @doc "Returns `b(not (left and not right))`"
  def left_implies_right(left, right) do
    b(not (left and not right))
  end

  # Temporarily making this private so that the function can be moved without
  # a major version bump in the SAT refactoring PR.
  # TODO: Make public with #2375 
  @doc false
  @spec generate_expression(StreamData.t(term())) :: StreamData.t(boolean_expr())
  def generate_expression(inner_generator) do
    inner_generator = StreamData.one_of([StreamData.boolean(), inner_generator])

    StreamData.tree(inner_generator, fn child_expr ->
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

  @doc """
  Transforms a statement to Conjunctive Normal Form(CNF), as lists of lists of integers.
  """
  def to_cnf(expression) do
    expression_with_constants = b(true and not false and expression)

    {bindings, expression} = extract_bindings(expression_with_constants)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> Enum.map(fn scenario ->
      Enum.sort_by(scenario, fn item ->
        {abs(item), item}
      end)
    end)
    |> group_predicates(bindings)
    |> rebind()
    |> unique_clauses()
  end

  defp unique_clauses({clauses, bindings}) do
    {Enum.uniq(clauses), bindings}
  end

  defp group_predicates(expression, bindings) do
    case expression do
      [_] ->
        {expression, bindings}

      scenarios ->
        Enum.reduce(scenarios, {[], bindings}, fn scenario, {new_scenarios, bindings} ->
          {scenario, bindings} = group_scenario_predicates(scenario, scenarios, bindings)
          {[scenario | new_scenarios], bindings}
        end)
    end
  end

  defp group_scenario_predicates(scenario, all_scenarios, bindings) do
    scenario
    |> Ash.SatSolver.Utils.ordered_sublists()
    |> Enum.filter(&can_be_used_as_group?(&1, all_scenarios, bindings))
    |> Enum.sort_by(&length/1)
    |> remove_overlapping()
    |> Enum.reduce({scenario, bindings}, fn group, {scenario, bindings} ->
      bindings = add_group_binding(bindings, group)

      {Ash.SatSolver.Utils.replace_ordered_sublist(scenario, group, bindings[:groups][group]),
       bindings}
    end)
  end

  defp remove_overlapping([]), do: []

  defp remove_overlapping([item | rest]) do
    if Enum.any?(item, fn n ->
         Enum.any?(rest, &(n in &1 or -n in &1))
       end) do
      remove_overlapping(rest)
    else
      [item | remove_overlapping(rest)]
    end
  end

  @doc false
  @spec unbind([[integer()]], map()) :: {[[integer()]], map()}
  def unbind(expression, %{temp_bindings: temp_bindings, old_bindings: old_bindings}) do
    expression =
      Enum.flat_map(expression, fn statement ->
        Enum.flat_map(statement, fn var ->
          neg? = var < 0
          old_binding = temp_bindings[abs(var)]

          case old_bindings[:reverse_groups][old_binding] do
            nil ->
              if neg? do
                [-old_binding]
              else
                [old_binding]
              end

            group ->
              if neg? do
                Enum.map(group, &(-&1))
              else
                [{:expand, group}]
              end
          end
        end)
        |> expand_groups()
      end)

    {expression, old_bindings}
  end

  defp expand_groups(expression) do
    do_expand_groups(expression)
  end

  defp do_expand_groups([]), do: [[]]

  defp do_expand_groups([{:expand, group} | rest]) do
    Enum.flat_map(group, fn var ->
      Enum.map(do_expand_groups(rest), fn future ->
        [var | future]
      end)
    end)
  end

  defp do_expand_groups([var | rest]) do
    Enum.map(do_expand_groups(rest), fn future ->
      [var | future]
    end)
  end

  defp rebind({expression, bindings}) do
    {expression, temp_bindings} =
      Enum.reduce(expression, {[], %{current: 0}}, fn statement, {statements, acc} ->
        {statement, acc} =
          Enum.reduce(statement, {[], acc}, fn var, {statement, acc} ->
            case acc[:reverse][abs(var)] do
              nil ->
                binding = acc.current + 1

                value =
                  if var < 0 do
                    -binding
                  else
                    binding
                  end

                {[value | statement],
                 acc
                 |> Map.put(:current, binding)
                 |> Map.update(:reverse, %{abs(var) => binding}, &Map.put(&1, abs(var), binding))
                 |> Map.put(binding, abs(var))}

              value ->
                value =
                  if var < 0 do
                    -value
                  else
                    value
                  end

                {[value | statement], acc}
            end
          end)

        {[Enum.reverse(statement) | statements], acc}
      end)

    bindings_with_old_bindings = %{temp_bindings: temp_bindings, old_bindings: bindings}

    {expression, bindings_with_old_bindings}
  end

  defp can_be_used_as_group?(group, scenarios, bindings) do
    Map.has_key?(bindings[:groups] || %{}, group) ||
      Enum.all?(scenarios, fn scenario ->
        has_no_overlap?(scenario, group) || group_in_scenario?(scenario, group)
      end)
  end

  defp has_no_overlap?(scenario, group) do
    not Enum.any?(group, fn group_predicate ->
      Enum.any?(scenario, fn scenario_predicate ->
        abs(group_predicate) == abs(scenario_predicate)
      end)
    end)
  end

  defp group_in_scenario?(scenario, group) do
    Ash.SatSolver.Utils.is_ordered_sublist_of?(group, scenario)
  end

  defp add_group_binding(bindings, group) do
    if bindings[:groups][group] do
      bindings
    else
      binding = bindings[:current]

      bindings
      |> Map.put_new(:reverse_groups, %{})
      |> Map.update!(:reverse_groups, &Map.put(&1, binding, group))
      |> Map.put_new(:groups, %{})
      |> Map.update!(:groups, &Map.put(&1, group, binding))
      |> Map.put(:current, binding + 1)
    end
  end

  @doc false
  @spec solutions_to_predicate_values([integer()], map()) :: map()
  def solutions_to_predicate_values(solution, bindings) do
    Enum.reduce(solution, %{true: [], false: []}, fn var, state ->
      fact = Map.get(bindings, abs(var))

      if is_nil(fact) do
        raise Ash.Error.Framework.AssumptionFailed.exception(
                message: """
                A fact from the SAT solver had no corresponding bound fact:

                Bindings:
                  #{inspect(bindings)}

                Missing:
                  #{inspect(var)}
                """
              )
      end

      Map.put(state, fact, var > 0)
    end)
  end

  defp extract_bindings(expr, bindings \\ %{current: 1})

  defp extract_bindings({operator, left, right}, bindings) do
    {bindings, left_extracted} = extract_bindings(left, bindings)
    {bindings, right_extracted} = extract_bindings(right, bindings)

    {bindings, {operator, left_extracted, right_extracted}}
  end

  defp extract_bindings({:not, value}, bindings) do
    {bindings, extracted} = extract_bindings(value, bindings)

    {bindings, b(not extracted)}
  end

  defp extract_bindings(value, %{current: current} = bindings) do
    current_binding =
      Enum.find(bindings, fn {key, binding_value} ->
        key != :current && binding_value == value
      end)

    case current_binding do
      nil ->
        new_bindings =
          bindings
          |> Map.put(:current, current + 1)
          |> Map.put(current, value)

        {new_bindings, current}

      {binding, _} ->
        {bindings, binding}
    end
  end

  # A helper function for formatting to the same output we'd give to picosat
  @doc false
  def to_picosat(clauses, variable_count) do
    clause_count = Enum.count(clauses)

    formatted_input =
      Enum.map_join(clauses, "\n", fn clause ->
        format_clause(clause) <> " 0"
      end)

    "p cnf #{variable_count} #{clause_count}\n" <> formatted_input
  end

  defp negations_to_negative_numbers(clauses) do
    Enum.map(
      clauses,
      fn
        {:not, var} when is_integer(var) ->
          [negate_var(var)]

        var when is_integer(var) ->
          [var]

        clause ->
          Enum.map(clause, fn
            {:not, var} -> negate_var(var)
            var -> var
          end)
      end
    )
  end

  defp negate_var(var, multiplier \\ -1)

  defp negate_var({:not, value}, multiplier) do
    negate_var(value, multiplier * -1)
  end

  defp negate_var(value, multiplier), do: value * multiplier

  defp format_clause(clause) do
    Enum.map_join(clause, " ", fn
      {:not, var} -> "-#{var}"
      var -> "#{var}"
    end)
  end

  defp lift_clauses({:and, left, right}) do
    lift_clauses(left) ++ lift_clauses(right)
  end

  defp lift_clauses({:or, left, right}) do
    [lift_or_clauses(left) ++ lift_or_clauses(right)]
  end

  defp lift_clauses(value), do: [[value]]

  defp lift_or_clauses({:or, left, right}) do
    lift_or_clauses(left) ++ lift_or_clauses(right)
  end

  defp lift_or_clauses(value), do: [value]

  defp to_conjunctive_normal_form(expression) do
    expression
    |> demorgans_law()
    |> distributive_law()
  end

  defp distributive_law(expression) do
    distributive_law_applied = apply_distributive_law(expression)

    if expression == distributive_law_applied do
      expression
    else
      distributive_law(distributive_law_applied)
    end
  end

  defp apply_distributive_law({:or, left, {:and, right1, right2}}) do
    left_distributed = apply_distributive_law(left)

    {:and, {:or, left_distributed, apply_distributive_law(right1)},
     {:or, left_distributed, apply_distributive_law(right2)}}
  end

  defp apply_distributive_law({:or, {:and, left1, left2}, right}) do
    right_distributed = apply_distributive_law(right)

    {:and, {:or, apply_distributive_law(left1), right_distributed},
     {:or, apply_distributive_law(left2), right_distributed}}
  end

  defp apply_distributive_law({:not, expression}) do
    {:not, apply_distributive_law(expression)}
  end

  defp apply_distributive_law({operator, left, right}) when operator in [:and, :or] do
    {operator, apply_distributive_law(left), apply_distributive_law(right)}
  end

  defp apply_distributive_law(var) when is_integer(var) do
    var
  end

  defp demorgans_law(expression) do
    demorgans_law_applied = apply_demorgans_law(expression)

    if expression == demorgans_law_applied do
      expression
    else
      demorgans_law(demorgans_law_applied)
    end
  end

  defp apply_demorgans_law({:not, {:and, left, right}}) do
    {:or, {:not, apply_demorgans_law(left)}, {:not, apply_demorgans_law(right)}}
  end

  defp apply_demorgans_law({:not, {:or, left, right}}) do
    {:and, {:not, left}, {:not, right}}
  end

  defp apply_demorgans_law({operator, left, right}) when operator in [:or, :and] do
    {operator, apply_demorgans_law(left), apply_demorgans_law(right)}
  end

  defp apply_demorgans_law({:not, expression}) do
    {:not, apply_demorgans_law(expression)}
  end

  defp apply_demorgans_law(var) when is_integer(var) do
    var
  end
end
