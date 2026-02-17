<!--
  SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
  SPDX-License-Identifier: MIT
-->

<!-- ex_doc_ignore_start -->
# Crux
<!-- ex_doc_ignore_end -->

![Elixir CI](https://github.com/ash-project/crux/workflows/Ash%20CI/badge.svg)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/ash-project/crux/badge)](https://scorecard.dev/viewer/?uri=github.com/ash-project/crux)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/crux.svg)](https://hex.pm/packages/crux)
[![Hexdocs badge](https://img.shields.io/badge/docs-hexdocs-purple)](https://hexdocs.pm/crux)
[![REUSE status](https://api.reuse.software/badge/github.com/ash-project/crux)](https://api.reuse.software/info/github.com/ash-project/crux)
[![Crux DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/ash-project/crux)

Crux is a powerful Elixir library for boolean satisfiability (SAT) solving,
boolean expression manipulation, and constraint satisfaction. It provides an
intuitive DSL for creating boolean expressions and a comprehensive toolkit for
working with satisfiability problems.

## Features

- **Boolean Expression DSL** - Intuitive macro for creating complex boolean
  expressions
- **SAT Solving** - Solve satisfiability problems with multiple backend solvers
- **Expression Manipulation** - Simplify, evaluate, and transform boolean
  expressions
- **CNF Conversion** - Convert expressions to Conjunctive Normal Form for SAT
  solving
- **Decision Trees** - Build binary decision trees for exploring satisfying
  assignments
- **Constraint Helpers** - Built-in functions for common constraint patterns
- **Multiple Backends** - Support for PicoSAT (fast NIF) and SimpleSAT (pure
  Elixir)

## Installation

Add `crux` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:crux, "~> 0.1.2"},
    # Choose one SAT solver backend:
    {:picosat_elixir, "~> 0.2"}, # Recommended: Fast NIF-based solver
    # OR
    {:simple_sat, "~> 0.1"}      # Pure Elixir alternative
  ]
end
```

## Quick Start

### Creating Boolean Expressions

Use the `b/1` macro to create boolean expressions with an intuitive syntax:

```elixir
import Crux.Expression

# Basic boolean operations
expr = b(:user_logged_in and (:is_admin or :is_moderator))

# Advanced boolean operators
expr = b(xor(:payment_cash, :payment_card))  # exactly one payment method
expr = b(implies(:is_student, :gets_discount)) # if student then discount
```

### Solving Satisfiability Problems

Convert expressions to formulas and solve them:

```elixir
alias Crux.{Expression, Formula}

# Create and solve a formula
expression = Expression.b(:a and (:b or :c))
formula = Formula.from_expression(expression)

case Crux.solve(formula) do
  {:ok, solution} ->
    IO.inspect(solution) # %{a: true, b: true, c: false}
  {:error, :unsatisfiable} ->
    IO.puts("No solution exists")
end
```

### Expression Manipulation

```elixir
import Crux.Expression

# Simplify expressions
complex_expr = b((:a and true) or (false and :b))
simple_expr = Expression.simplify(complex_expr)  # :a

# Evaluate with variable assignments
result = Expression.run(b(:a and :b), fn
  :a -> true
  :b -> false
end)  # false

# Convert to Conjunctive Normal Form
cnf_expr = Expression.to_cnf(b(:a or (:b and :c)))
```

## Core Concepts

### Expressions

Boolean expressions are the foundation of Crux. They support:
- Variables (atoms like `:user`, `:admin`)
- Constants (`true`, `false`)
- Basic operators (`and`, `or`, `not`)
- Advanced operators (`xor`, `nand`, `nor`, `implies`, `implied_by`, `xnor`)

### Formulas

Formulas are expressions converted to Conjunctive Normal Form (CNF) for SAT solving:

```elixir
formula = Formula.from_expression(Expression.b(:a and :b))
# %Formula{
#   cnf: [[1], [2]],
#   bindings: %{1 => :a, 2 => :b},
#   reverse_bindings: %{a: 1, b: 2}
# }
```

### SAT Solving

Crux can determine if boolean formulas are satisfiable and find satisfying assignments:

```elixir
# Check satisfiability
Crux.satisfiable?(formula)  # true/false

# Find all satisfying scenarios
Crux.satisfying_scenarios(formula)  # [%{a: true, b: true}]

# Build decision trees
tree = Crux.decision_tree(formula)  # {:a, false, {:b, false, true}}
```

## API Overview

### Core Modules

- **`Crux`** - Main SAT solving functions (`solve/1`, `satisfiable?/1`, `decision_tree/2`)
- **`Crux.Expression`** - Boolean expression creation and manipulation
- **`Crux.Formula`** - CNF formula representation and conversion

### Expression Functions

```elixir
# Creation
Expression.b(:a and :b)

# Manipulation
Expression.simplify/1      # Simplify expressions
Expression.to_cnf/1        # Convert to CNF
Expression.balance/1       # Normalize operand order

# Evaluation
Expression.run/2           # Evaluate with variable bindings
Expression.expand/2        # Expand with custom callbacks

# Traversal
Expression.prewalk/2       # Pre-order traversal
Expression.postwalk/2      # Post-order traversal

# Constraint helpers
Expression.at_most_one/1   # At most one variable true
Expression.exactly_one/1   # Exactly one variable true
Expression.all_or_none/1   # All variables same value
```

## SAT Solver Backends

Crux supports multiple SAT solver backends:

### PicoSAT (Recommended)

```elixir
{:picosat_elixir, "~> 0.2"}
```
- Fast NIF-based solver
- Production-ready and battle-tested
- Best performance for large problems

### SimpleSAT

```elixir
{:simple_sat, "~> 0.1"}
```
- Pure Elixir implementation
- No NIF dependencies
- Suitable for smaller problems or when avoiding NIFs

## Advanced Features

### Decision Trees

Build binary decision trees to explore all satisfying assignments:

```elixir
formula = Formula.from_expression(Expression.b(:a and :b))
tree = Crux.decision_tree(formula, sorter: &<=/2)
# {:a, false, {:b, false, true}}
```

### Constraint Patterns

Crux provides helpers for common constraint satisfaction patterns:

```elixir
import Crux.Expression

# User can have at most one role
roles = [:admin, :moderator, :user]
at_most_one_role = at_most_one(roles)

# Payment methods - exactly one must be selected
payment_methods = [:cash, :card, :paypal]
payment_constraint = exactly_one(payment_methods)

# Feature flags - all related features synchronized
related_features = [:dark_mode_ui, :dark_mode_api]
sync_constraint = all_or_none(related_features)
```

### Domain Knowledge Integration

Provide custom conflict and implication rules for domain-specific validation:

```elixir
opts = [
  conflicts?: fn
    :admin, :guest -> true    # admin and guest roles conflict
    _, _ -> false
  end,
  implies?: fn
    :admin, :can_delete -> true  # admin implies delete permission
    _, _ -> false
  end
]

scenarios = Crux.satisfying_scenarios(formula, opts)
```

## Use Cases

### Authorization Policies

Model complex authorization rules:

```elixir
import Crux.Expression

# User access policy
policy = b(
  (:is_owner or :is_admin) and
  not :is_suspended and
  (:has_subscription or :is_trial_user)
)

# Check if a specific user satisfies the policy
user_context = %{
  is_owner: false,
  is_admin: true,
  is_suspended: false,
  has_subscription: true,
  is_trial_user: false
}

result = Expression.run(policy, fn var -> Map.get(user_context, var, false) end)

case result do
  true -> :access_granted
  false -> :access_denied
end
```

### Resource Scheduling

Model resource allocation constraints:

```elixir
# Meeting room scheduling
rooms = [:room_a, :room_b, :room_c]
time_slots = [:slot_1, :slot_2, :slot_3]

constraints = for room <- rooms do
  # Each room can be booked at most once per time slot
  at_most_one(for slot <- time_slots, do: :"#{room}_#{slot}")
end
```

<!-- ex_doc_ignore_start -->
## License

This project is licensed under the MIT License - see the [LICENSES](LICENSES)
directory for details.
<!-- ex_doc_ignore_end -->