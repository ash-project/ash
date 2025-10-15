# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicySimple.Context.Changes.DoChange do
  @moduledoc false

  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      name = Ash.Changeset.get_argument(changeset, :name)
      Ash.Changeset.force_change_attribute(changeset, :name, name)
    end)
  end
end
