defmodule Ash.Error.Changes.NoSuchRelationship do
  @moduledoc "Used when a change is provided for an relationship that does not exist"

  use Splode.Error, fields: [:resource, :relationship], class: :invalid

  def message(error) do
    "No such relationship #{error.relationship} for resource #{inspect(error.resource)}"
  end
end
