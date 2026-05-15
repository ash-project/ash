# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.EmbeddedArrayElementField do
  @moduledoc """
  An internal expression node representing a reference to a field of the
  *current element* being iterated inside an `exists/2` over an embedded
  array attribute.

  Not constructed by users. Produced by `Ash.Filter` as part of the
  AST-rewrite step that prepares an `Ash.Query.Exists` whose first path
  segment is an embedded-array attribute, so that downstream compilers
  (e.g. AshPostgres) can translate the field reference into a
  `jsonb_extract_path_text(elem.value, 'field')::type` style expression.
  """

  defstruct [:field, :type, :constraints]

  @type t :: %__MODULE__{
          field: atom(),
          type: Ash.Type.t() | nil,
          constraints: keyword()
        }

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{field: field}, _opts) do
      concat(["#elem.", to_string(field)])
    end
  end
end
