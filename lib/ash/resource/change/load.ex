# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.Load do
  @moduledoc false
  use Ash.Resource.Change

  @impl true
  def temporal_safe?(_opts), do: true

  alias Ash.Changeset

  @impl true
  def change(changeset, opts, _context) do
    Changeset.load(changeset, opts[:target])
  end

  @impl true
  def atomic(changeset, opts, _context) do
    {:atomic, Changeset.load(changeset, opts[:target]), %{}}
  end
end
