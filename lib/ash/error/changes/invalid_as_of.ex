# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Changes.InvalidAsOf do
  @moduledoc "Used when a write is given an `as_of` that does not make a valid period"

  use Splode.Error, fields: [:resource, :as_of, :message], class: :invalid

  def message(error) do
    "Cannot write #{inspect(error.resource)} as of #{inspect(error.as_of)}: #{error.message}"
  end
end
