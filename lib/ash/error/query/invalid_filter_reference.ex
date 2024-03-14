defmodule Ash.Error.Query.InvalidFilterReference do
  @moduledoc "Used when an invalid reference is used in a filter"
  use Ash.Error.Exception

  use Splode.Error, fields: [:field, :simple_equality?], class: :invalid

  def splode_message(%{field: field, simple_equality?: true}) do
    "#{field} cannot be referenced in filters, except by simple equality"
  end

  def splode_message(%{field: field}) do
    "#{field} cannot be referenced in filters"
  end
end
