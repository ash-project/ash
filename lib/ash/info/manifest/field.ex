# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Field do
  @moduledoc """
  Represents a resource field (attribute, calculation, or aggregate) in the API specification.
  """

  @type kind :: :attribute | :calculation | :aggregate

  @type t :: %__MODULE__{
          name: atom(),
          kind: kind(),
          type: Ash.Info.Manifest.Type.t(),
          allow_nil?: boolean(),
          writable?: boolean(),
          has_default?: boolean(),
          description: String.t() | nil,
          filterable?: boolean(),
          sortable?: boolean(),
          primary_key?: boolean(),
          sensitive?: boolean(),
          select_by_default?: boolean(),
          # For calculations only
          arguments: [Ash.Info.Manifest.Argument.t()] | nil,
          # For aggregates only
          aggregate_kind: atom() | nil,
          # Resolved at generation time; nil iff filterable? == false.
          # Each entry carries the operator/function/expression name plus its
          # pre-resolved right-hand-side type — consumers don't need to walk
          # signatures again.
          filter_operators: [Ash.Info.Manifest.ApplicableOperator.t()] | nil,
          filter_functions: [Ash.Info.Manifest.ApplicableFunction.t()] | nil,
          filter_custom_expressions: [Ash.Info.Manifest.ApplicableCustomExpression.t()] | nil,
          custom: map()
        }

  defstruct [
    :name,
    :kind,
    :type,
    :allow_nil?,
    :writable?,
    :has_default?,
    :description,
    :filterable?,
    :sortable?,
    :primary_key?,
    :sensitive?,
    :select_by_default?,
    :arguments,
    :aggregate_kind,
    :filter_operators,
    :filter_functions,
    :filter_custom_expressions,
    custom: %{}
  ]
end
