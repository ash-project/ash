defmodule Ash.Error.Query.NoSuchRelationship do
  @moduledoc "Used when an relationship that doesn't exist is used in a query"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :relationship], class: :invalid

  def message(error) do
    "No such relationship #{error.relationship} for resource #{inspect(error.resource)}"
  end
end
