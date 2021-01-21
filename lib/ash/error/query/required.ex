defmodule Ash.Error.Query.Required do
  @moduledoc "Used when a filter or argument is required in a query"
  use Ash.Error

  def_ash_error([:field, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "query_required"

    def message(error) do
      "#{error.type} #{error.field} is required"
    end
  end
end
