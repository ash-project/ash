# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.GetAndLockTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :count, :integer, default: 0, public?: true
    end

    actions do
      defaults [:read, create: [:count]]

      update :get_and_lock_for_update do
        change get_and_lock_for_update()
        change atomic_update(:count, expr(count + 1))
      end

      update :get_and_lock_for_update_skip_atomic do
        change get_and_lock_for_update(skip_atomic?: true)
        change atomic_update(:count, expr(count + 1))
      end

      update :get_and_lock do
        change get_and_lock(:for_update)
        change atomic_update(:count, expr(count + 1))
      end

      update :get_and_lock_skip_atomic do
        change get_and_lock(:for_update, skip_atomic?: true)
        change atomic_update(:count, expr(count + 1))
      end
    end
  end

  for action <- [:get_and_lock_for_update, :get_and_lock] do
    test "#{action} can't be done atomically" do
      assert {:not_atomic, message} =
               Ash.Changeset.fully_atomic_changeset(Post, unquote(action), %{})

      assert message =~ "Cannot lock during an atomic update"
    end

    test "#{action} with `skip_atomic?: true` is skipped when done atomically" do
      assert %Ash.Changeset{before_action: []} =
               Ash.Changeset.fully_atomic_changeset(Post, :"#{unquote(action)}_skip_atomic", %{})
    end
  end
end
