defmodule Ash.Test.Actions.BulkUpdateManualTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule UpdateManual do
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
    end

    def bulk_update(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.update(opts ++ [return_notifications?: true])
        |> case do
          {:ok, record, notification} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_update_index,
                changeset.context.bulk_update.index
              )

            [{:ok, record, notification} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule UpdateManualMapReturn do
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
    end

    def bulk_update(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.update(opts ++ [return_notifications?: true])
        |> case do
          {:ok, record, notifications} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_update_index,
                changeset.context.bulk_update.index
              )

            [{:ok, record, %{notifications: notifications}} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule UpdateManualNoNotifications do
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
    end

    def bulk_update(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.update(opts)
        |> case do
          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_update_index,
                changeset.context.bulk_update.index
              )

            [{:ok, record} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule UpdateManualTupledNotifications do
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
    end

    def bulk_update(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.update(opts ++ [return_notifications?: true])
        |> case do
          {:ok, _record, notifications} ->
            [{:notifications, notifications} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule UpdateManualOk do
    use Ash.Resource.ManualUpdate

    def update(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.update(opts)
    end

    def bulk_update(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        changeset.data
        |> Ash.Changeset.for_update(:update, Map.take(changeset.attributes, [:name]), opts)
        |> Ash.update(opts ++ [return_notifications?: true])
        |> case do
          {:ok, _record, _notifications} ->
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
      defaults [:read, :destroy]

      update :update_manual do
        require_atomic? false
        accept [:name]
        manual UpdateManual
      end

      update :update_manual_map_return do
        require_atomic? false
        accept [:name]
        manual UpdateManualMapReturn
      end

      update :update_manual_no_notifications do
        require_atomic? false
        accept [:name]
        manual UpdateManualNoNotifications
      end

      update :update_manual_tupled_notifications do
        require_atomic? false
        accept [:name]
        manual UpdateManualTupledNotifications
      end

      update :update_manual_ok do
        require_atomic? false
        accept [:name]
        manual UpdateManualOk
      end

      create :create do
        accept [:name]
      end

      update :update do
        require_atomic? false
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

  test "bulk_update works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_update with return_notifications?: false works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual, %{name: "updated name"},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_update with return_records?: false works on manual action returning notifications as list" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_update works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_map_return, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_update with return_notifications?: false works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_map_return, %{name: "updated name"},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_update with return_records?: false works on manual action returning notifications in map" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_map_return, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
  end

  test "bulk_update works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_no_notifications, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_update with return_notifications?: false works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_no_notifications, %{name: "updated name"},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_update with return_records?: false works on manual action not returning notifications at all" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_no_notifications, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_update works on manual action returning notifications as tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_tupled_notifications, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.empty?(result.records)
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_update return_notifications?: false works on manual action returning notifications as tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_tupled_notifications, %{name: "updated name"},
        return_notifications?: false,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.empty?(result.records)
    assert Enum.empty?(result.notifications)
    assert result.error_count == 0
  end

  test "bulk_update return_records?: false works on manual action returning notifications as tuple" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_tupled_notifications, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_update works on manual action returning :ok" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    result =
      authors
      |> Ash.bulk_update(:update_manual_ok, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_update raises when manual action returns :ok when return_records?: true" do
    authors = [
      create_author("Author1"),
      create_author("Author2"),
      create_author("Author3")
    ]

    assert_raise(Ash.Error.Unknown, fn ->
      authors
      |> Ash.bulk_update(:update_manual_ok, %{name: "updated name"},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )
    end)
  end
end
