defmodule Ash.Error.Changes.InvalidAttribute do
  @moduledoc "Used when an invalid value is provided for an attribute change"
  use Ash.Error.Exception

  use Splode.Error, fields: [:field, :message, :private_vars, :value], class: :invalid

  def message(error) do
    """
    Invalid value provided#{for_field(error)}#{do_message(error)}

    #{inspect(error.value)}
    """
  end

  defp for_field(%{field: field}) when not is_nil(field), do: " for #{field}"
  defp for_field(_), do: ""

  defp do_message(%{message: ""} = error), do: do_message(%{error | message: nil})

  defp do_message(%{message: message}) when is_binary(message) do
    ": #{message}."
  end

  defp do_message(_other) do
    "."
  end
end
