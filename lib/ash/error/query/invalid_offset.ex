# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidOffset do
  @moduledoc "Used when an invalid offset is provided"

  use Splode.Error, fields: [:offset], class: :invalid

  def message(%{offset: offset}) do
    "#{inspect(offset)} is not a valid offset"
  end
end
