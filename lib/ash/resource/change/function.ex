# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.Function do
  @moduledoc false
  use Ash.Resource.Change

  # Wraps an arbitrary function, whose temporal safety cannot be known.
  @impl true
  def temporal_safe?(_opts), do: false

  @impl true
  def change(changeset, [{:fun, {m, f, a}}], context) do
    apply(m, f, [changeset, context | a])
  end

  def change(changeset, [{:fun, fun}], context) do
    fun.(changeset, context)
  end
end
