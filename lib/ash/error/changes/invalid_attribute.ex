defmodule Ash.Error.Changes.InvalidAttribute do
  @moduledoc "Used when an invalid value is provided for an attribute change"
  use Ash.Error

  def_ash_error([:field, :type, :message, :validation], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_attribute"

    def message(error) do
      "Invalid value#{for_type(error)}provided#{for_field(error)}#{do_message(error)}"
    end

    defp for_field(%{field: field}) when not is_nil(field), do: " for #{field}"
    defp for_field(_), do: ""
    defp for_type(%{type: type}) when not is_nil(type), do: " for #{type} "
    defp for_type(_), do: " "

    defp do_message(%{message: message}) when not is_nil(message) do
      ": #{message}."
    end

    defp do_message(_), do: "."
  end
end
