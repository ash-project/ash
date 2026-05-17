# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.FilterCapabilities do
  @moduledoc """
  Top-level filter vocabulary for a manifest.

  Carries every operator, function, and registered custom expression
  available to filter expressions in this app. The `predicate_*` lists are
  denormalized convenience views: the names of entries whose `predicate?` is
  `true` — i.e. the leaf-usable subset of each catalog.
  """

  alias Ash.Info.Manifest.{CustomExpression, Function, Operator}

  @type t :: %__MODULE__{
          operators: [Operator.t()],
          functions: [Function.t()],
          custom_expressions: [CustomExpression.t()],
          boolean_connectives: [atom()],
          predicate_operators: [atom()],
          predicate_functions: [atom()],
          predicate_custom_expressions: [atom()],
          custom: map()
        }

  defstruct operators: [],
            functions: [],
            custom_expressions: [],
            boolean_connectives: [:and, :or, :not],
            predicate_operators: [],
            predicate_functions: [],
            predicate_custom_expressions: [],
            custom: %{}
end
