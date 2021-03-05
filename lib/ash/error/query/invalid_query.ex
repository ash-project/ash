defmodule Ash.Error.Query.InvalidQuery do
  @moduledoc """
  A generic error that can be used to add an error to a query for a specific field
  """
  use Ash.Error.Exception

  def_ash_error([:field, :message], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_query"

    def message(error) do
      error.message
    end
  end
end
