# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidCalculationArgument do
  @moduledoc "Used when an invalid value is provided for a calculation argument"

  use Splode.Error, fields: [:calculation, :field, :message, :value], class: :invalid

  def message(error) do
    """
    Invalid value provided for calculation argument #{error.field} in #{error.calculation}#{do_message(error)}

    #{inspect(error.value)}
    """
  end

  defp do_message(%{message: message}) when not is_nil(message) do
    ": #{message}."
  end

  defp do_message(_), do: "."
end
