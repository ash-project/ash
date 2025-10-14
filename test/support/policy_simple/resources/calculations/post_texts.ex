# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicySimple.Calculations.PostTexts do
  @moduledoc false
  use Ash.Resource.Calculation

  @impl true
  def init(opts) do
    {:ok, opts}
  end

  @impl true
  def load(_query, _opts, _context) do
    [posts: [:text]]
  end

  @impl true
  def calculate(records, _opts, %{}) do
    Enum.map(records, fn record ->
      Enum.map(record.posts, fn post ->
        post.text
      end)
    end)
  end
end
