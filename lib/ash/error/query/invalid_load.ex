# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidLoad do
  @moduledoc "Used when an invalid load is provided"

  use Splode.Error, fields: [:load], class: :invalid

  def message(%{load: load}) do
    "#{inspect(load)} is not a valid load"
  end
end
