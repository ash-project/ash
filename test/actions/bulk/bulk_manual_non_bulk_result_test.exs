# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkManualNonBulkResultTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
      :ok
    end
  end

  # No `bulk_create/3`, so a batch goes through `process_non_bulk_result/6`.
  defmodule CreateManualMapNotifications do
    @moduledoc false
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts =
        ctx
        |> Ash.Context.to_opts()
        |> Keyword.put(:return_notifications?, true)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
      |> case do
        {:ok, record, notifications} -> {:ok, record, %{notifications: notifications}}
        other -> other
      end
    end
  end

  # No `bulk_update/3`, same as above.
  defmodule UpdateManualMapNotifications do
    @moduledoc false
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts =
        ctx
        |> Ash.Context.to_opts()
        |> Keyword.put(:return_notifications?, true)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
      |> case do
        {:ok, record, notifications} -> {:ok, record, %{notifications: notifications}}
        other -> other
      end
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy]

      create :create do
        accept [:name]
      end

      update :update do
        accept [:name]
      end

      create :create_manual do
        accept [:name]
        manual CreateManualMapNotifications
      end

      update :update_manual do
        accept [:name]
        require_atomic? false
        manual UpdateManualMapNotifications
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
        allow_nil?(false)
      end
    end
  end

  defp notification_shapes(%Ash.BulkResult{notifications: notifications}) do
    Enum.map(notifications, fn
      %Ash.Notifier.Notification{} -> Ash.Notifier.Notification
      other when is_map(other) -> {:unwrapped_map, Map.keys(other)}
      other -> {:unexpected, other}
    end)
  end

  test "bulk_create on a manual action without a bulk callback returns its notifications" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}]
      |> Ash.bulk_create(Author, :create_manual,
        return_notifications?: true,
        return_records?: true,
        return_errors?: true,
        authorize?: false
      )

    assert %Ash.BulkResult{status: :success} = result
    assert Enum.count(result.records) == 2

    assert Enum.uniq(notification_shapes(result)) == [Ash.Notifier.Notification]
  end

  test "bulk_update on a manual action without a bulk callback returns its notifications" do
    authors =
      [%{name: "Author1"}, %{name: "Author2"}]
      |> Ash.bulk_create!(Author, :create,
        return_records?: true,
        authorize?: false
      )
      |> Map.fetch!(:records)

    result =
      Ash.bulk_update(authors, :update_manual, %{name: "Updated"},
        return_notifications?: true,
        return_records?: true,
        return_errors?: true,
        strategy: :stream,
        authorize?: false
      )

    assert %Ash.BulkResult{status: :success} = result
    assert Enum.count(result.records) == 2

    assert Enum.uniq(notification_shapes(result)) == [Ash.Notifier.Notification]
  end

  test "the same manual modules still work through the single-record pipelines" do
    author =
      Author
      |> Ash.Changeset.for_create(:create_manual, %{name: "Author"}, authorize?: false)
      |> Ash.create!()

    assert author.name == "Author"

    updated =
      author
      |> Ash.Changeset.for_update(:update_manual, %{name: "Updated"}, authorize?: false)
      |> Ash.update!()

    assert updated.name == "Updated"
  end
end
