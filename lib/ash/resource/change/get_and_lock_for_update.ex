# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.GetAndLockForUpdate do
  @moduledoc """
  Refetches the record being updated or destroyed, and locks it for update.

  Equivalent to `Ash.Resource.Change.GetAndLock` with `lock: :for_update`.
  """
  use Ash.Resource.Change

  alias Ash.Resource.Change.GetAndLock

  @impl true
  def temporal_safe?(_opts), do: true

  @impl true
  def change(changeset, opts, context) do
    GetAndLock.change(changeset, for_update(opts), context)
  end

  @impl true
  def atomic(changeset, opts, context) do
    GetAndLock.atomic(changeset, for_update(opts), context)
  end

  defp for_update(opts), do: Keyword.put(opts, :lock, :for_update)
end
