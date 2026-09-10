# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.TemporalNotSupported do
  @moduledoc "Used when the data layer of a temporal resource cannot serve an `as_of`"

  use Splode.Error, fields: [:resource, :as_of], class: :invalid

  def message(%{resource: resource, as_of: as_of}) do
    "Data layer for #{inspect(resource)} does not support temporal resources, so it cannot serve as_of: #{inspect(as_of)}"
  end
end
