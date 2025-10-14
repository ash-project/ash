# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidFilterReference do
  @moduledoc "Used when an invalid reference is used in a filter"

  use Splode.Error, fields: [:field, :simple_equality?], class: :invalid

  def message(%{field: field, simple_equality?: true}) do
    "#{field} cannot be referenced in filters, except by simple equality"
  end

  def message(%{field: field}) do
    "#{field} cannot be referenced in filters"
  end
end
