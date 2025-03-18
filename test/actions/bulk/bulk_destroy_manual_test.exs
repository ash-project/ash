defmodule Ash.Test.Actions.BulkDestroyManualTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule DestroyManualNotificationList do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.destroy(opts ++ [return_notifications?: true, return_destroyed?: true])
        |> case do
          {:ok, record, notification} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record, notification} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule DestroyManualMapReturn do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.destroy(opts ++ [return_notifications?: true, return_destroyed?: true])
        |> case do
          {:ok, record, notifications} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record, %{notifications: notifications}} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule DestroyManualNoNotifications do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.destroy(opts ++ [return_destroyed?: true])
        |> case do
          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule DestroyManualTupledNotifications do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.destroy(opts ++ [return_notifications?: true])
        |> case do
          {:ok, notifications} ->
            [{:notifications, notifications} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule DestroyManualOk do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.destroy(opts)
        |> case do
          :ok ->
            [:ok | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :update, :destroy]

      destroy :destroy_manual_notification_list do
        accept [:name]
        manual DestroyManualNotificationList
      end

      destroy :destroy_manual_map_return do
        accept [:name]
        manual DestroyManualMapReturn
      end

      destroy :destroy_manual_no_notifications do
        accept [:name]
        manual DestroyManualNoNotifications
      end

      destroy :destroy_manual_tupled_notifications do
        accept [:name]
        manual DestroyManualTupledNotifications
      end

      destroy :destroy_manual_ok do
        accept [:name]
        manual DestroyManualOk
      end

      create :create do
        accept [:name]
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
        allow_nil? false
      end
    end
  end

  def create_author(name) do
    Author
    |> Ash.Changeset.for_create(:create, %{name: name})
    |> Ash.create!()
  end

  test "bulk_destroy works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_notification_list, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_destroy with return_notifications?: false works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_notification_list, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_destroy with return_records?: false works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_notification_list, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_destroy works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_map_return, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_destroy with return_notifications?: false works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_map_return, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_destroy with return_records?: false works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_map_return, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_destroy works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_no_notifications, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_destroy with return_notifications?: false works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_no_notifications, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_destroy with return_records?: false works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_no_notifications, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_destroy works on manual action returning notifications in tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.empty?(result.records)
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_destroy with return_notifications?: false works on manual action returning notifications in tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.empty?(result.records)
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_destroy with return_records?: false works on manual action returning notifications in tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_destroy doesn't return notifications when return_notifications?: false" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.empty?(result.records)
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_destroy works on manual action returning :ok" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    # Should this be nil when return_notifications?: true?
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy raises if manual action doesn't return records" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    assert_raise(RuntimeError, fn ->
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )
    end)
  end
end
