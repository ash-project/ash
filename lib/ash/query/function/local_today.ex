# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.LocalToday do
  @moduledoc """
  Returns the current date in the given time zone.

  For example, `local_today("Asia/Hong_Kong")`. Temporal queries use their
  `as_of` instant. See `Ash.Query.Function.LocalNow` for time zone configuration.
  """
  use Ash.Query.Function, name: :local_today, eager_evaluate?: false

  def args, do: [[:string]]

  def returns, do: [:date]

  def evaluate(function) do
    with {:known, datetime} <- Ash.Query.Function.LocalNow.evaluate(function) do
      {:known, NaiveDateTime.to_date(datetime)}
    end
  end
end
