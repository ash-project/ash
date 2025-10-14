# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.NoSuchOperator do
  @moduledoc "Used when an operator that doesn't exist is used in a query"

  use Splode.Error, fields: [:operator], class: :invalid

  def message(error) do
    "No such operator #{error.operator}"
  end
end
