defmodule Ash.Error.Changes.InvalidRelationship do
  @moduledoc "Used when an invalid value is provided for a relationship change"
  use Ash.Error.Exception

  def_ash_error([:relationship, :message], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_relationship"

    def message(error) do
      "Invalid value provided#{for_relationship(error)}#{do_message(error)}"
    end

    defp for_relationship(%{relationship: relationship}) when not is_nil(relationship),
      do: " for #{relationship}"

    defp for_relationship(_), do: ""

    defp do_message(%{message: message}) when is_binary(message) do
      ": #{message}."
    end

    defp do_message(%{message: message}) when not is_nil(message) do
      ": #{inspect(message)}."
    end

    defp do_message(_), do: "."
  end
end
