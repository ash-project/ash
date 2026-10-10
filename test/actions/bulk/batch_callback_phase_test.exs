# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.BatchCallbackPhaseTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule RecordPhases do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _opts, _context), do: changesets

    @impl true
    def atomic(changeset, _opts, _context), do: {:ok, changeset}

    @impl true
    def before_batch(changesets, _opts, _context) do
      Enum.map(changesets, fn changeset ->
        send(self(), {:before_batch, changeset.action_type, changeset.phase})
        changeset
      end)
    end

    @impl true
    def after_batch(changesets_and_results, _opts, _context) do
      Enum.map(changesets_and_results, fn {changeset, result} ->
        send(self(), {:after_batch, changeset.action_type, changeset.phase})
        {:ok, result}
      end)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      defaults [:read]

      create :create do
        primary? true
        accept [:title]
        change RecordPhases
      end

      update :update do
        accept [:title]
        change RecordPhases
      end

      destroy :destroy do
        change RecordPhases
      end
    end
  end

  defp create_posts do
    Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Post, :create, return_records?: true)
  end

  test "bulk create" do
    create_posts()

    assert_received {:before_batch, :create, :before_action}
    assert_received {:after_batch, :create, :after_action}
    refute_received {_, :create, :pending}
  end

  test "bulk update" do
    create_posts()

    Ash.bulk_update!(Post, :update, %{title: "c"}, strategy: :stream)

    assert_received {:before_batch, :update, :before_action}
    assert_received {:after_batch, :update, :after_action}
    refute_received {_, :update, :pending}
  end

  test "bulk destroy" do
    create_posts()

    Ash.bulk_destroy!(Post, :destroy, %{}, strategy: :stream)

    assert_received {:before_batch, :destroy, :before_action}
    assert_received {:after_batch, :destroy, :after_action}
    refute_received {_, :destroy, :pending}
  end

  # atomic batches have no changeset per record, so only `after_batch/3` runs
  test "atomic bulk update" do
    create_posts()

    Ash.bulk_update!(Post, :update, %{title: "c"}, strategy: :atomic_batches)

    assert_received {:after_batch, :update, :after_action}
    refute_received {_, :update, :pending}
  end

  test "atomic bulk destroy" do
    create_posts()

    Ash.bulk_destroy!(Post, :destroy, %{}, strategy: :atomic_batches)

    assert_received {:after_batch, :destroy, :after_action}
    refute_received {_, :destroy, :pending}
  end

  test "a single create running batch callbacks" do
    Ash.create!(Post, %{title: "a"})

    assert_received {:before_batch, :create, :before_action}
    assert_received {:after_batch, :create, :after_action}
  end
end
