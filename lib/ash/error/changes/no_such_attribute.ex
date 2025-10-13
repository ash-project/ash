# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Changes.NoSuchAttribute do
  @moduledoc "Used when a change is provided for an attribute that does not exist"

  use Splode.Error, fields: [:resource, :attribute], class: :invalid

  def message(error) do
    "No such attribute #{error.attribute} for resource #{inspect(error.resource)}"
  end
end
