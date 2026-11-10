# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.AfterBatchNotificationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    @impl true
    def notify(%{metadata: %{batch: titles}}), do: send(self(), {:batch, Enum.sort(titles)})
    def notify(_notification), do: :ok
  end

  defmodule NotifyOncePerBatch do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _opts, _context), do: changesets

    @impl true
    def after_batch([{changeset, _} | _] = changesets_and_results, _opts, _context) do
      results = Enum.map(changesets_and_results, fn {_changeset, result} -> {:ok, result} end)

      notification = %Ash.Notifier.Notification{
        resource: changeset.resource,
        action: changeset.action,
        metadata: %{batch: Enum.map(changesets_and_results, fn {_, result} -> result.title end)},
        for: [Notifier]
      }

      results ++ [notification]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        accept [:title]
      end

      update :update do
        require_atomic? false
        accept [:title]
        change NotifyOncePerBatch
      end

      update :update_where do
        require_atomic? false
        accept [:title]
        change NotifyOncePerBatch, where: [attribute_equals(:title, "b")]
      end
    end
  end

  defp create_posts(titles) do
    Ash.bulk_create!(Enum.map(titles, &%{title: &1}), Post, :create, return_records?: true).records
  end

  test "a notification returned after a batch's results is sent, and the results are kept" do
    posts = create_posts(["a", "b", "c"])

    assert %{status: :success, records: records} =
             Ash.bulk_update(posts, :update, %{},
               strategy: :stream,
               notify?: true,
               return_records?: true
             )

    assert length(records) == 3
    assert_received {:batch, ["a", "b", "c"]}
  end

  test "a notification from a change scoped by `where` is sent for the records it applied to" do
    posts = create_posts(["a", "b"])

    assert %{status: :success} =
             Ash.bulk_update(posts, :update_where, %{}, strategy: :stream, notify?: true)

    assert_received {:batch, ["b"]}
  end
end
