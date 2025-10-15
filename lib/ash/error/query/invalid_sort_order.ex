# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidSortOrder do
  @moduledoc "Used when an invalid sort order is provided"

  use Splode.Error, fields: [:order], class: :invalid

  def message(%{order: order}) do
    "No such sort order #{inspect(order)}"
  end
end
