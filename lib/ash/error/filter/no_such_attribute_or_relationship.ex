defmodule Ash.Error.Filter.NoSuchAttributeOrRelationship do
  @moduledoc "Used when a key in a filter contains something that is neither an attribute or a relationship"
  use Ash.Error

  def_ash_error([:message, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_attribute_or_relationship"

    def class(_), do: :invalid

    def message(%{value: value, message: message}) do
      "Invalid filter value `#{inspect(value)}`. " <> message
    end

    def stacktrace(_), do: nil
  end
end
