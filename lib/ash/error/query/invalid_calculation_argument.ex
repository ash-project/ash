defmodule Ash.Error.Query.InvalidCalculationArgument do
  @moduledoc "Used when an invalid value is provided for a calculation argument"
  use Ash.Error.Exception

  def_ash_error([:calculation, :field, :message, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_calculation_argument"

    def message(error) do
      """
      Invalid value provided for calculation argument #{error.field} in #{error.calculation}: #{do_message(error)}

      #{inspect(error.value)}
      """
    end

    defp do_message(%{message: message}) when not is_nil(message) do
      ": #{message}."
    end

    defp do_message(_), do: "."
  end
end
