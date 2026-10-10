# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.BatchCallbackInitTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule RecordOpts do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def init(opts), do: {:ok, Keyword.put(opts, :initialized?, true)}

    @impl true
    def change(changeset, opts, _context) do
      send(self(), {:change, opts[:initialized?]})
      changeset
    end

    @impl true
    def batch_change(changesets, opts, _context) do
      send(self(), {:batch_change, opts[:initialized?]})
      changesets
    end

    @impl true
    def atomic(changeset, opts, _context) do
      send(self(), {:atomic, opts[:initialized?]})
      {:ok, changeset}
    end

    @impl true
    def before_batch(changesets, opts, _context) do
      send(self(), {:before_batch, opts[:initialized?]})
      changesets
    end

    @impl true
    def after_batch(changesets_and_results, opts, _context) do
      send(self(), {:after_batch, opts[:initialized?]})
      Enum.map(changesets_and_results, fn {_changeset, result} -> {:ok, result} end)
    end
  end

  # without `change/3`, so single actions run the batch callbacks too
  defmodule RecordBatchOpts do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def init(opts), do: {:ok, Keyword.put(opts, :initialized?, true)}

    @impl true
    def batch_change(changesets, opts, _context) do
      send(self(), {:batch_change, opts[:initialized?]})
      changesets
    end

    @impl true
    def before_batch(changesets, opts, _context) do
      send(self(), {:before_batch, opts[:initialized?]})
      changesets
    end

    @impl true
    def after_batch(changesets_and_results, opts, _context) do
      send(self(), {:after_batch, opts[:initialized?]})
      Enum.map(changesets_and_results, fn {_changeset, result} -> {:ok, result} end)
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
        change RecordOpts
      end

      create :create_with_batch_callbacks do
        accept [:title]
        change RecordBatchOpts
      end

      update :update do
        accept [:title]
        change RecordOpts
      end

      destroy :destroy do
        change RecordOpts
      end
    end
  end

  defp create_posts do
    Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Post, :create, return_records?: true)
  end

  defp callbacks_run do
    {:messages, messages} = Process.info(self(), :messages)
    for {callback, initialized?} <- messages, do: {callback, initialized?}
  end

  defp flush do
    receive do
      _ -> flush()
    after
      0 -> :ok
    end
  end

  # every callback should receive the options returned by `init/1`
  defp assert_initialized(callbacks) do
    assert callbacks != []
    assert Enum.reject(callbacks, fn {_callback, initialized?} -> initialized? end) == []
  end

  test "a single create" do
    Ash.create!(Post, %{title: "a"})
    assert_initialized(callbacks_run())
  end

  test "a single create running batch callbacks" do
    Post |> Ash.Changeset.for_create(:create_with_batch_callbacks, %{title: "a"}) |> Ash.create!()

    callbacks = callbacks_run()
    assert {:after_batch, true} in callbacks
    assert_initialized(callbacks)
  end

  test "bulk create" do
    create_posts()
    assert_initialized(callbacks_run())
  end

  test "bulk update" do
    create_posts()
    flush()

    Ash.bulk_update!(Post, :update, %{title: "c"}, strategy: :stream)
    assert_initialized(callbacks_run())
  end

  test "atomic bulk update" do
    create_posts()
    flush()

    Ash.bulk_update!(Post, :update, %{title: "c"}, strategy: :atomic_batches)
    assert_initialized(callbacks_run())
  end

  test "bulk destroy" do
    create_posts()
    flush()

    Ash.bulk_destroy!(Post, :destroy, %{}, strategy: :stream)
    assert_initialized(callbacks_run())
  end

  test "atomic bulk destroy" do
    create_posts()
    flush()

    Ash.bulk_destroy!(Post, :destroy, %{}, strategy: :atomic_batches)
    assert_initialized(callbacks_run())
  end
end
