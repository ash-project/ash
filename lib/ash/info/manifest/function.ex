# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Function do
  @moduledoc """
  Represents a single function in the filter capabilities catalog.

  Built from `Ash.Filter.builtin_functions/0` and from the `functions/1`
  callback of each data layer reachable from the manifest's resources.

  `signatures` may be the atom `:var_args` for functions that accept arbitrary
  arity (e.g. `fragment`).

  `data_layer_module` is `nil` for Ash builtins and a module atom for entries
  sourced from a specific data layer (e.g. `AshPostgres.DataLayer`).
  Consumers can use it to intersect a resource's data layer against the
  catalog when deciding which functions to render for that resource.
  """

  alias Ash.Info.Manifest.ArgumentSignature

  @type t :: %__MODULE__{
          name: atom(),
          module: module(),
          predicate?: boolean(),
          signatures: [ArgumentSignature.t()] | :var_args,
          returns: ArgumentSignature.arg_spec() | :unknown,
          description: String.t() | nil,
          data_layer_module: module() | nil,
          custom: map()
        }

  defstruct [
    :name,
    :module,
    :predicate?,
    :signatures,
    :returns,
    :description,
    :data_layer_module,
    custom: %{}
  ]
end
