# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.InvalidCustomInput do
  @moduledoc "Used when an invalid value is provided for a code interface custom input"

  use Splode.Error, fields: [:field, :message, :value], class: :invalid

  def message(error) do
    """
    Invalid value provided#{for_field(error)}#{do_message(error)}

    #{inspect(error.value)}
    """
  end

  defp for_field(%{field: field}) when not is_nil(field), do: " for #{field}"
  defp for_field(_), do: ""

  defp do_message(%{message: message}) when not is_nil(message) do
    ": #{message}."
  end

  defp do_message(_), do: "."
end
