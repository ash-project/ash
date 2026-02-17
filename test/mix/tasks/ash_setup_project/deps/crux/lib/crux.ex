# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

readme_path = Path.join(__DIR__, "../README.md")

readme_content =
  readme_path
  |> File.read!()
  |> String.replace(~r/<!-- ex_doc_ignore_start -->.*?<!-- ex_doc_ignore_end -->/s, "")

defmodule Crux do
  @moduledoc """
  #{readme_content}
  """

  alias Crux.Expression
  alias Crux.Formula

  require Expression

  @external_resource readme_path

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

  @simple_true Formula.simple_true()
  @simple_false Formula.simple_false()

  @doc """
  Solves a SAT formula and returns a satisfying assignment.

  Returns `{:ok, solution}` with a map of variable assignments, or
  `{:error, :unsatisfiable}` if no solution exists.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      ...> Crux.solve(formula)
      {:ok, %{a: true, b: true}}

      iex> formula = Formula.from_expression(Expression.b(:a and not :a))
      ...> Crux.solve(formula)
      {:error, :unsatisfiable}

  """
  @spec solve(Formula.t(variable)) :: {:ok, %{variable => boolean()}} | {:error, :unsatisfiable}
        when variable: term()
  def solve(formula)
  def solve(@simple_true), do: {:ok, %{}}
  def solve(@simple_false), do: {:error, :unsatisfiable}

  def solve(%Formula{cnf: cnf, bindings: bindings}) do
    case Crux.Implementation.solve_expression(cnf) do
      {:ok, solution} -> {:ok, unbind_scenario(solution, bindings)}
      {:error, :unsatisfiable} = error -> error
    end
  end

  @doc """
  Returns true if the formula is satisfiable (has at least one solution), false otherwise.

  ## Examples

      iex> formula = Formula.from_expression(Expression.b(:a or :b))
      ...> Crux.satisfiable?(formula)
      true

      iex> formula = Formula.from_expression(Expression.b(:a and not :a))
      ...> Crux.satisfiable?(formula)
      false

  """
  @spec satisfiable?(Formula.t()) :: boolean()
  def satisfiable?(formula)
  def satisfiable?(@simple_true), do: true
  def satisfiable?(@simple_false), do: false

  def satisfiable?(%Formula{cnf: cnf}) do
    case Crux.Implementation.solve_expression(cnf) do
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
      ...> Crux.decision_tree(formula)
      {:a, false, {:b, false, true}}

      iex> formula = Formula.from_expression(Expression.b(:a and :b))
      ...> Crux.decision_tree(formula, sorter: &>=/2)
      {:b, false, {:a, false, true}}

  """
  @spec decision_tree(Formula.t(variable), opts(variable)) :: tree(variable)
        when variable: term()
  def decision_tree(formula, opts \\ [])
  def decision_tree(@simple_true, _opts), do: true
  def decision_tree(@simple_false, _opts), do: false

  def decision_tree(formula, opts) do
    sorter = Keyword.get(opts, :sorter, &<=/2)
    variables = formula.bindings |> Map.values() |> Enum.sort(sorter)
    scenarios = satisfying_scenarios(formula, opts)
    build_decision_tree(variables, [], scenarios, opts)
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
      ...> Crux.satisfying_scenarios(formula)
      [%{a: true, b: true}]

      iex> formula = Formula.from_expression(Expression.b(:a or :b))
      ...> Crux.satisfying_scenarios(formula) |> Enum.sort()
      [%{a: true}, %{b: true}]

  """
  @spec satisfying_scenarios(Formula.t(variable), opts(variable)) :: [%{variable => boolean()}]
        when variable: term()
  def satisfying_scenarios(formula, opts \\ [])
  def satisfying_scenarios(@simple_true, _opts), do: [%{}]
  def satisfying_scenarios(@simple_false, _opts), do: []

  def satisfying_scenarios(formula, opts) do
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
      ...>
      ...> opts = [
      ...>   implies?: fn
      ...>     :a, :c -> true
      ...>     _, _ -> false
      ...>   end
      ...> ]
      ...>
      ...> Crux.validate_assignments(assignments, opts)
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
    case Crux.Implementation.solve_expression(working_formula.cnf) do
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
    Enum.flat_map(variable_ids, fn var_id ->
      cond do
        var_id in solver_model -> [var_id]
        -var_id in solver_model -> [-var_id]
        true -> []
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

      case Crux.Implementation.solve_expression(query_cnf) do
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
      cnf
      |> Enum.zip(selector_vars)
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
    %{formula | cnf: [blocking_clause | formula.cnf]}
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

  # Builds a decision tree by filtering pre-computed satisfying scenarios.
  # This approach avoids expensive SAT solving at each node by working with
  # known satisfying assignments and using early termination when a scenario
  # is already covered by the current path.
  @spec build_decision_tree(
          [variable],
          [{variable, boolean()}],
          [%{variable => boolean()}],
          opts(variable)
        ) :: tree(variable)
        when variable: term()
  defp build_decision_tree(_vars, _path, [], _opts), do: false

  defp build_decision_tree(vars, path, scenarios, opts) do
    if Enum.any?(scenarios, &covered_by_path?(&1, path)) do
      true
    else
      case vars do
        [] ->
          true

        [variable | rest] ->
          case branch_shortcuts_with_validate(variable, path, opts) do
            :force_true ->
              right_scenarios = filter_and_strip(scenarios, variable, true)
              build_decision_tree(rest, [{variable, true} | path], right_scenarios, opts)

            :force_false ->
              left_scenarios = filter_and_strip(scenarios, variable, false)
              build_decision_tree(rest, [{variable, false} | path], left_scenarios, opts)

            :both ->
              left_scenarios = filter_and_strip(scenarios, variable, false)
              right_scenarios = filter_and_strip(scenarios, variable, true)

              left = build_decision_tree(rest, [{variable, false} | path], left_scenarios, opts)
              right = build_decision_tree(rest, [{variable, true} | path], right_scenarios, opts)

              if left == right, do: left, else: {variable, left, right}

            :unsat ->
              false
          end
      end
    end
  end

  # Determines the branching strategy for a variable based on validation results.
  # Uses conflict and implication analysis to skip impossible branches early.
  @spec branch_shortcuts_with_validate(variable, [{variable, boolean()}], opts(variable)) ::
          :force_true | :force_false | :both | :unsat
        when variable: term()
  defp branch_shortcuts_with_validate(variable, path, opts) do
    case {validate_assignment(path, {variable, false}, opts),
          validate_assignment(path, {variable, true}, opts)} do
      {:conflict, :conflict} -> :unsat
      {:conflict, _} -> :force_true
      {_, :implied} -> :force_true
      {_, :conflict} -> :force_false
      {:ok, :ok} -> :both
    end
  end

  # Filters scenarios to those compatible with variable=value, removing the variable
  # from matching scenarios to enable early coverage detection downstream.
  @spec filter_and_strip([%{variable => boolean()}], variable, boolean()) :: [
          %{variable => boolean()}
        ]
        when variable: term()
  defp filter_and_strip(scenarios, variable, value) do
    Enum.reduce(scenarios, [], fn scenario, acc ->
      case scenario do
        %{^variable => ^value} -> [Map.delete(scenario, variable) | acc]
        %{^variable => other} when other != value -> acc
        # don’t-care
        _missing -> [scenario | acc]
      end
    end)
  end

  # Returns true if the current path covers all variable assignments in the scenario.
  # Used for early termination when a satisfying assignment is already guaranteed.
  @spec covered_by_path?(%{variable => boolean()}, [{variable, boolean()}]) :: boolean()
        when variable: term()
  defp covered_by_path?(scenario, path) do
    path_map = Map.new(path)
    Enum.all?(scenario, fn {variable, value} -> Map.get(path_map, variable) == value end)
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
end
