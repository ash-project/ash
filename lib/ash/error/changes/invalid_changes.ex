defmodule Ash.Error.Changes.InvalidChanges do
  @moduledoc "Used when a change is provided that covers multiple attributes/relationships"
  use Ash.Error.Exception

  def_ash_error([:fields, :message, :validation], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_attribute"

    def message(error) do
      "#{for_fields(error)}#{do_message(error)}"
    end

    defp for_fields(%{fields: fields}) when not is_nil(fields) do
      "#{Enum.join(fields, ", ")}"
    end

    defp for_fields(_), do: ""

    defp do_message(%{message: message}) when not is_nil(message) do
      ": #{message}."
    end

    defp do_message(_), do: "."
  end
end
