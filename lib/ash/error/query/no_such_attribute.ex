# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.NoSuchAttribute do
  @moduledoc "Used when an attribute that doesn't exist is used in a query"

  use Splode.Error, fields: [:resource, :attribute], class: :invalid

  def message(error) do
    "No such attribute #{error.attribute} for resource #{inspect(error.resource)}"
  end
end
