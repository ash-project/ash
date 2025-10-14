# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Forbidden.ForbiddenField do
  @moduledoc "Raised in cases where access to a specific field was prevented"

  require Logger

  use Splode.Error, fields: [:resource, :field], class: :forbidden

  def message(error) do
    "Forbidden: cannot access #{error.field} on #{inspect(error.resource)}"
  end
end
