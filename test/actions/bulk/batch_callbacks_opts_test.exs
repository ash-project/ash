# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.BatchCallbacksOptsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  # Records the options each callback receives, which should always be the options returned by
  # `init/1`, with templates filled.
  defmodule RecordOpts do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def init(opts), do: {:ok, Keyword.put(opts, :initialized?, true)}

    @impl true
    def batch_callbacks?(_changesets_or_query, opts, _context) do
      record(:batch_callbacks?, opts)
      true
    end

    @impl true
    def batch_change(changesets, opts, _context) do
      record(:batch_change, opts)
      changesets
    end

    @impl true
    def atomic(changeset, opts, _context) do
      record(:atomic, opts)
      {:ok, changeset}
    end

    @impl true
    def before_batch(changesets, opts, _context) do
      record(:before_batch, opts)
      changesets
    end

    @impl true
    def after_batch(changesets_and_results, opts, _context) do
      record(:after_batch, opts)
      Enum.map(changesets_and_results, fn {_changeset, result} -> {:ok, result} end)
    end

    defp record(callback, opts) do
      send(self(), {:opts, callback, opts})
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
        change {RecordOpts, by: actor(:name)}
      end

      update :update do
        accept [:title]
        change {RecordOpts, by: actor(:name)}
      end

      destroy :destroy do
        change {RecordOpts, by: actor(:name)}
      end
    end
  end

  @actor %{name: "zach"}

  defp create_posts do
    Ash.bulk_create!([%{title: "a"}, %{title: "b"}], Post, :create, actor: @actor)
  end

  defp flush do
    receive do
      _ -> flush()
    after
      0 -> :ok
    end
  end

  # every callback receives a keyword list from `init/1`, with the actor template filled
  defp assert_opts_ready do
    {:messages, messages} = Process.info(self(), :messages)
    received = for {:opts, callback, opts} <- messages, do: {callback, opts}

    assert received != []

    for {callback, opts} <- received do
      assert Keyword.keyword?(opts), "#{callback} received #{inspect(opts)}"
      assert opts[:initialized?], "#{callback} received options without init/1: #{inspect(opts)}"
      assert opts[:by] == "zach", "#{callback} received unfilled templates: #{inspect(opts)}"
    end
  end

  test "a single create" do
    Ash.create!(Post, %{title: "a"}, actor: @actor)
    assert_opts_ready()
  end

  test "bulk create" do
    create_posts()
    assert_opts_ready()
  end

  for strategy <- [:stream, :atomic, :atomic_batches] do
    test "bulk update with the #{strategy} strategy" do
      create_posts()
      flush()

      Ash.bulk_update!(Post, :update, %{title: "c"}, strategy: unquote(strategy), actor: @actor)
      assert_opts_ready()
    end

    test "bulk destroy with the #{strategy} strategy" do
      create_posts()
      flush()

      Ash.bulk_destroy!(Post, :destroy, %{}, strategy: unquote(strategy), actor: @actor)
      assert_opts_ready()
    end
  end
end
