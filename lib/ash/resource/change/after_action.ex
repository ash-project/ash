# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.AfterAction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @impl true
  def change(changeset, opts, context) do
    Ash.Changeset.after_action(
      changeset,
      fn changeset, result ->
        opts[:callback].(changeset, result, context)
      end,
      prepend?: opts[:prepend?]
    )
  end
end
