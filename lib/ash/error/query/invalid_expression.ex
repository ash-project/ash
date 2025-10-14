# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidExpression do
  @moduledoc "Used when an invalid expression is used in a filter"

  use Splode.Error, fields: [:expression, :message], class: :invalid

  def message(%{expression: expression, message: message}) do
    "invalid expression: #{inspect(expression)}: #{message}"
  end
end
