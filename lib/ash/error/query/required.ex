# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.Required do
  @moduledoc "Used when a filter or argument is required in a query"

  use Splode.Error, fields: [:field, :type, :resource], class: :invalid

  def message(error) do
    "#{error.type} #{error.field} is required"
  end
end
