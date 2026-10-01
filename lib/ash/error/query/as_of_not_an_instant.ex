# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.AsOfNotAnInstant do
  @moduledoc "Used when a read is asked for an `as_of` that names no instant to read at"

  use Splode.Error, fields: [:resource, :as_of], class: :invalid

  def message(error) do
    "Cannot read #{inspect(error.resource)} as of #{inspect(error.as_of)}, which names no instant"
  end
end
