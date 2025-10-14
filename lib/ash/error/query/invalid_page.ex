# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidPage do
  @moduledoc "Used when an invalid page option is provided"

  use Splode.Error, fields: [:page], class: :invalid

  def message(%{page: page}) do
    "#{inspect(page)} is not a valid page option"
  end
end
