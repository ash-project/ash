# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidFilterValue do
  @moduledoc "Used when an invalid value is provided for a filter"

  use Splode.Error, fields: [:message, :value, :context], class: :invalid

  def message(%{value: value, message: message, context: context})
      when not is_nil(context) do
    text =
      if context do
        "Invalid filter value `#{inspect(value)}` supplied in `#{inspect(context)}`"
      else
        "Invalid filter value `#{inspect(value)}`"
      end

    if message do
      text <> ": " <> message
    else
      text
    end
  end

  def message(%{value: value, message: message}) do
    if message do
      "Invalid filter value `#{inspect(value)}`: " <> message
    else
      "Invalid filter value `#{inspect(value)}`."
    end
  end
end
