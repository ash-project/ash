# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.NoSuchRelationship do
  @moduledoc "Used when an relationship that doesn't exist is used in a query"

  use Splode.Error, fields: [:resource, :relationship], class: :invalid

  def message(error) do
    "No such relationship #{error.relationship} for resource #{inspect(error.resource)}"
  end
end
