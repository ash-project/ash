defmodule Ash.Error.Query.NoSuchAttributeOrRelationship do
  @moduledoc "Used when a key in a filter contains something that is neither an attribute or a relationship"
  use Ash.Error.Exception

  def_ash_error([:attribute_or_relationship, :resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_attribute_or_relationship"

    def message(%{attribute_or_relationship: attribute_or_relationship, resource: resource}) do
      "No such attribute or relationship #{inspect(attribute_or_relationship)} for #{inspect(resource)}"
    end
  end
end
