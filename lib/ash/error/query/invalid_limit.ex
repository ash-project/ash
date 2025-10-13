# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidLimit do
  @moduledoc "Used when an invalid limit is provided"

  use Splode.Error, fields: [:limit], class: :invalid

  def message(%{limit: limit}) do
    "#{inspect(limit)} is not a valid limit"
  end
end
