defmodule Ash.Test.Actions.BulkCreateManualTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule CreateManualNotificationList do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.resource
        |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.create(opts ++ [return_notifications?: true])
        |> case do
          {:ok, record, notification} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_create_index,
                changeset.context.bulk_create.index
              )

            [{:ok, record, notification} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule CreateManualMapReturn do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.resource
        |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.create(opts ++ [return_notifications?: true])
        |> case do
          {:ok, record, notifications} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_create_index,
                changeset.context.bulk_create.index
              )

            [{:ok, record, %{notifications: notifications}} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule CreateManualNoNotifications do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.resource
        |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.create(opts)
        |> case do
          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_create_index,
                changeset.context.bulk_create.index
              )

            [{:ok, record} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule CreateManualTupledNotifications do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.resource
        |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.create(opts ++ [return_notifications?: true])
        |> case do
          {:ok, _record, notifications} ->
            [{:notifications, notifications} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule CreateManualOk do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.resource
        |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.create(opts)
        |> case do
          {:ok, _record} ->
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

      create :create_manual_notification_list do
        accept [:name]
        manual CreateManualNotificationList
      end

      create :create_manual_map_return do
        accept [:name]
        manual CreateManualMapReturn
      end

      create :create_manual_no_notifications do
        accept [:name]
        manual CreateManualNoNotifications
      end

      create :create_manual_tupled_notifications do
        accept [:name]
        manual CreateManualTupledNotifications
      end

      create :create_manual_ok do
        accept [:name]
        manual CreateManualOk
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

  test "bulk_create works on manual action returning notifications as list" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_notification_list,
        return_notifications?: true,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_create with return_notifications?: false works on manual action returning notifications as list" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_notification_list,
        return_notifications?: false,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 0
    assert result.error_count == 0
  end

  test "bulk_create with return_records?: false works on manual action returning notifications as list" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_notification_list,
        return_notifications?: true,
        return_errors?: true,
        return_records?: false
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_create works on manual action returning notifications in map" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_map_return,
        return_notifications?: true,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_create with return_notifications?: false works on manual action returning notifications in map" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_map_return,
        return_notifications?: false,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 0
    assert result.error_count == 0
  end

  test "bulk_create with return_records?: false works on manual action returning notifications in map" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_map_return,
        return_notifications?: true,
        return_errors?: true,
        return_records?: false
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_create works on manual action not returning notifications at all" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_no_notifications,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 0
    assert result.error_count == 0
  end

  test "bulk_create with return_records?: false works on manual action not returning notifications at all" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_no_notifications,
        return_errors?: true,
        return_records?: false
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 0
    assert result.error_count == 0
  end

  test "bulk_create works on manual action returning notifications in tuple" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_tupled_notifications,
        return_errors?: true,
        return_records?: true,
        return_notifications?: true
      )

    assert Enum.count(result.records) == 0
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_create with return_notifications?: false works on manual action returning notifications in tuple" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_tupled_notifications,
        return_errors?: true,
        return_records?: true,
        return_notifications?: false
      )

    assert Enum.count(result.records) == 0
    assert Enum.count(result.notifications) == 0
    assert result.error_count == 0
  end

  test "bulk_create works on manual action returning :ok" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_ok,
        return_errors?: true,
        return_records?: false,
        return_notifications?: true
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_create raises when manual action returns :ok and return_records?: true" do
    assert_raise(Ash.Error.Unknown, fn ->
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_ok,
        return_errors?: true,
        return_records?: true,
        return_notifications?: true
      )
    end)
  end

  test "bulk_create on manual action with bulk_create/3 fails with invalid inputs" do
    result =
      [%{name: "Author1"}, %{}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual_notification_list,
        return_notifications?: true,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.empty?(result.records)
    assert result.notifications == nil
    assert result.error_count == 1
  end
end
