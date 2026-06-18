# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.UpsertConflict do
  @moduledoc """
  Refers to the *incoming* value of a field during an upsert.

  Only valid in expressions evaluated during an upsert, such as an `upsert_condition`.
  When an upsert finds a conflicting record, a bare field reference refers to the value
  *already in the database*, while `upsert_conflict(:field)` refers to the value that the
  current create attempted to set.

  For example, to skip the upsert unless the incoming `contents` differ from the stored value:

      upsert_condition expr(contents != upsert_conflict(:contents))

  See the [create actions guide](https://hexdocs.pm/ash/create-actions.html#upserts) for more.
  """

  defstruct [:attribute]

  def new(attribute) when is_atom(attribute) do
    %__MODULE__{attribute: attribute}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{attribute: attribute}, opts) do
      concat([
        "upsert_conflict(",
        to_doc(attribute, opts),
        ")"
      ])
    end
  end
end
