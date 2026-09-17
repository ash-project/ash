# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidSort do
  @moduledoc "Used when an invalid sort is provided"

  use Splode.Error, fields: [:sort], class: :invalid

  def message(%{sort: sort}) do
    "#{inspect(sort)} is not a valid sort"
  end
end
