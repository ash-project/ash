defmodule Ash.Error.Query.InvalidFilterReference do
  @moduledoc "Used when an invalid reference is used in a filter"
  use Ash.Error.Exception

  def_ash_error([:field, :simple_equality?], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_filter_reference"

    def class(_), do: :invalid

    def message(%{field: field, simple_equality?: true}) do
      "#{field} cannot be referenced in filters, except by simple equality"
    end

    def message(%{field: field}) do
      "#{field} cannot be referenced in filters"
    end

    def stacktrace(_), do: nil
  end
end
