# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.LimitRequired do
  @moduledoc "Used when no limit is provided, pagination is required, and no default page size is configured"

  use Splode.Error, fields: [], class: :invalid

  def message(_) do
    "Limit is required"
  end
end
