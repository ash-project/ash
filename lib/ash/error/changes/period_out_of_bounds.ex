# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Changes.PeriodOutOfBounds do
  @moduledoc "Used when a write names a period reaching beyond the version it supersedes"

  use Splode.Error, fields: [:resource, :field, :period, :within], class: :invalid

  def message(error) do
    "Cannot write #{inspect(error.field)} over #{inspect(error.period)} on #{inspect(error.resource)}: " <>
      "it reaches beyond #{inspect(error.within)}, the version it supersedes"
  end
end
