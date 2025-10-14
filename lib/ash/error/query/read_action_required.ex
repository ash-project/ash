# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.ReadActionRequired do
  @moduledoc "Used when a relationship is filtered and the destination does not have a default read action"

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    """
    A default read action is required on the destination in order to filter
    on a relationship. Destination: #{inspect(resource)}
    """
  end
end
