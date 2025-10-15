# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Changes.InvalidAttribute do
  @moduledoc "Used when an invalid value is provided for an attribute change"

  use Splode.Error,
    fields: [:field, :message, :private_vars, :value, has_value?: false],
    class: :invalid

  def exception(opts) do
    super(Keyword.put(opts, :has_value?, Keyword.has_key?(opts, :value)))
  end

  def message(error) do
    value = if error.has_value?, do: "\n\nValue: " <> inspect(error.value), else: ""

    """
    Invalid value provided#{for_field(error)}#{do_message(error)}#{value}
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
