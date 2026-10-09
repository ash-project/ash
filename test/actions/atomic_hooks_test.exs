# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.AtomicHooksTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule BeforeActionInAtomic do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        send(self(), {:before_action, changeset.action.name})
        changeset
      end)
    end

    @impl true
    def atomic(changeset, opts, context), do: {:ok, change(changeset, opts, context)}
  end

  defmodule AfterActionInAtomic do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.after_action(changeset, fn changeset, result ->
        send(self(), {:after_action, changeset.action.name})
        {:ok, result}
      end)
    end

    @impl true
    def atomic(changeset, opts, context), do: {:ok, change(changeset, opts, context)}
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :count, :integer, default: 0, public?: true
    end

    actions do
      defaults [:read, create: [:count]]

      update :bump do
        change BeforeActionInAtomic
        change atomic_update(:count, expr(count + 1))
      end

      update :bump_non_atomic do
        require_atomic? false
        change BeforeActionInAtomic
        change atomic_update(:count, expr(count + 1))
      end

      update :bump_after_action do
        change AfterActionInAtomic
        change atomic_update(:count, expr(count + 1))
      end

      destroy :destroy do
        primary? true
        change BeforeActionInAtomic
      end

      destroy :destroy_non_atomic do
        require_atomic? false
        change BeforeActionInAtomic
      end
    end
  end

  test "a change that adds a before_action hook in atomic/3 can't be done atomically" do
    assert {:not_atomic, reason} = Ash.Changeset.fully_atomic_changeset(Post, :bump, %{})
    assert reason =~ inspect(BeforeActionInAtomic)
    assert reason =~ "before_action"
  end

  test "an action requiring atomicity refuses it rather than skipping the hook" do
    post = Ash.create!(Post, %{})

    assert_raise Ash.Error.Framework, ~r/must be performed atomically/, fn ->
      post |> Ash.Changeset.for_update(:bump) |> Ash.update!()
    end

    refute_received {:before_action, _}
  end

  test "an update that may be non-atomic runs the hook" do
    post = Ash.create!(Post, %{})

    assert %{count: 1} =
             post |> Ash.Changeset.for_update(:bump_non_atomic) |> Ash.update!()

    assert_received {:before_action, :bump_non_atomic}
  end

  test "a bulk update falls back from the atomic strategy and runs the hook" do
    Ash.create!(Post, %{})

    assert %Ash.BulkResult{status: :success} =
             Ash.bulk_update!(Post, :bump_non_atomic, %{}, strategy: [:atomic, :stream])

    assert_received {:before_action, :bump_non_atomic}
  end

  test "a bulk destroy falls back from the atomic strategy and runs the hook" do
    Ash.create!(Post, %{})

    assert %Ash.BulkResult{status: :success} =
             Ash.bulk_destroy!(Post, :destroy_non_atomic, %{}, strategy: [:atomic, :stream])

    assert_received {:before_action, :destroy_non_atomic}
    assert [] = Ash.read!(Post)
  end

  test "an after_action hook added in atomic/3 still runs atomically" do
    post = Ash.create!(Post, %{})

    assert %Ash.Changeset{} = Ash.Changeset.fully_atomic_changeset(Post, :bump_after_action, %{})

    assert %{count: 1} =
             post |> Ash.Changeset.for_update(:bump_after_action) |> Ash.update!()

    assert_received {:after_action, :bump_after_action}
  end
end
