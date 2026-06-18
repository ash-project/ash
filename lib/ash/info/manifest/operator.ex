# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Operator do
  @moduledoc """
  Represents a single operator in the filter capabilities catalog.

  Built from `Ash.Filter.builtin_operators/0`.

  ## Canonical name vs. aliases

  `name` is the **user-facing operator symbol** — the spelling that appears
  in `Ash.Query.filter` expressions and that extensions should render to
  clients (e.g. `:==`, `:<`, `:in`, `:is_nil`).

  `aliases` carries the legacy/module-derived spellings (e.g. `:eq`,
  `:less_than`). New consumers should key off `name`. Tools that already
  render the module-derived names (e.g. some historical ash_graphql input
  shapes) can substitute via aliases for backward compatibility.

  Each per-field `%Ash.Info.Manifest.ApplicableOperator{}` records `name` (the
  canonical symbol), not an alias. Consumers look up the full operator
  definition via `Ash.Info.Manifest.operator_lookup/1`.
  """

  alias Ash.Info.Manifest.ArgumentSignature

  @type t :: %__MODULE__{
          name: atom(),
          module: module(),
          aliases: [atom()],
          predicate?: boolean(),
          signatures: [ArgumentSignature.t()],
          returns: ArgumentSignature.arg_spec() | :unknown,
          description: String.t() | nil,
          custom: map()
        }

  defstruct [
    :name,
    :module,
    :aliases,
    :predicate?,
    :signatures,
    :returns,
    :description,
    custom: %{}
  ]
end
