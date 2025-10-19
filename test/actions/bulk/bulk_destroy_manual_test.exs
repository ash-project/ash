# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkDestroyManualTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Actions.BulkDestroyManualTest.Helpers
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
      opts = Helpers.build_destroy_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, module_opts, ctx) do
      destroy_ctx = Helpers.build_destroy_ctx(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        destroy(changeset, module_opts, destroy_ctx)
        |> case do
          :ok ->
            [:ok | results]

          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record} | results]

          {:ok, record, notifications} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record, notifications} | results]

          {:error, error} ->
            [{:error, error} | results]
        end
      end)
    end
  end

  defmodule DestroyManualMapReturn do
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, ctx) do
      opts = Helpers.build_destroy_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, module_opts, ctx) do
      destroy_ctx = Helpers.build_destroy_ctx(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        destroy(changeset, module_opts, destroy_ctx)
        |> case do
          :ok ->
            [:ok | results]

          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record} | results]

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
      opts = Helpers.build_destroy_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, module_opts, ctx) do
      destroy_ctx = Helpers.build_destroy_ctx(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        destroy(changeset, module_opts, destroy_ctx)
        |> case do
          :ok ->
            [:ok | results]

          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record} | results]

          {:ok, record, _notifications} ->
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
      opts = Helpers.build_destroy_opts(ctx)

      changeset.data
      |> Ash.Changeset.for_destroy(:destroy, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.destroy(opts)
    end

    def bulk_destroy(changesets, module_opts, ctx) do
      destroy_ctx = Helpers.build_destroy_ctx(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        destroy(changeset, module_opts, destroy_ctx)
        |> case do
          :ok ->
            [:ok | results]

          {:ok, record} ->
            record =
              Ash.Resource.put_metadata(
                record,
                :bulk_destroy_index,
                changeset.context.bulk_destroy.index
              )

            [{:ok, record} | results]

          {:ok, _record, notifications} ->
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

  defmodule Helpers do
    def build_destroy_ctx(ctx) do
      %Ash.Resource.ManualDestroy.Context{
        actor: ctx.actor,
        tenant: ctx.tenant,
        select: ctx.select,
        tracer: ctx.tracer,
        authorize?: ctx.authorize?,
        domain: ctx.domain,
        return_notifications?: ctx.return_notifications?,
        return_destroyed?: ctx.return_records?
      }
    end

    def build_destroy_opts(ctx) do
      Ash.Context.to_opts(ctx)
      |> Keyword.put(:return_notifications?, ctx.return_notifications?)
      |> Keyword.put(:return_destroyed?, ctx.return_destroyed?)
    end
  end

  def create_author(name) do
    Author
    |> Ash.Changeset.for_create(:create, %{name: name})
    |> Ash.create!()
  end

  def create_authors do
    [create_author("Author1"), create_author("Author2"), create_author("Author3")]
  end

  test "bulk_destroy works on manual action returning notifications as list" do
    authors = create_authors()

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

  test "bulk_destroy with :destroy_manual_notification_list and return_records?: false doesn't return records" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_notification_list, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_notification_list and return_notifications?: true returns notifications" do
    authors = create_authors()

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

  test "bulk_destroy with return_errors?: false doesn't return errors" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_notification_list, %{},
        return_notifications?: true,
        return_errors?: false,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
    assert result.errors == nil
  end

  test "bulk_destroy with :destroy_manual_map_return works" do
    authors = create_authors()

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

  test "bulk_destroy with :destroy_manual_map_return and return_records?: false doesn't return records" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_map_return, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_map_return and return_notifications?: true returns notifications" do
    authors = create_authors()

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

  test "bulk_destroy with :destroy_manual_map_return and return_errors?: false doesn't return errors" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_map_return, %{},
        return_notifications?: true,
        return_errors?: false,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 6
    assert result.error_count == 0
    assert result.errors == nil
  end

  test "bulk_destroy with :destroy_manual_no_notifications works" do
    authors = create_authors()

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

  test "bulk_destroy with :destroy_manual_no_notifications and return_records?: false doesn't return records" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_no_notifications, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_no_notifications and return_notifications?: true returns notifications" do
    authors = create_authors()

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

  test "bulk_destroy with :destroy_manual_no_notifications and return_errors?: false doesn't return errors" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_no_notifications, %{},
        return_notifications?: true,
        return_errors?: false,
        return_records?: true,
        strategy: :stream
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
    assert result.errors == nil
  end

  test "bulk_destroy with :destroy_manual_tupled_notifications works" do
    authors = create_authors()

    assert_raise RuntimeError, fn ->
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: true,
        strategy: :stream
      )
    end
  end

  test "bulk_destroy with :destroy_manual_tupled_notifications and return_records?: false doesn't return records" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_tupled_notifications and return_notifications?: true returns notifications" do
    authors = create_authors()

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
    assert result.errors == []
  end

  test "bulk_destroy with :destroy_manual_tupled_notifications and return_errors?: false doesn't return errors" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_tupled_notifications, %{},
        return_notifications?: true,
        return_errors?: false,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
    assert result.errors == nil
  end

  test "bulk_destroy with :destroy_manual_ok works" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == []
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_ok and return_records?: false doesn't return records" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: false,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == nil
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_ok and return_notifications?: true returns notifications" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: true,
        return_errors?: true,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == []
    assert result.error_count == 0
  end

  test "bulk_destroy with :destroy_manual_ok and return_errors?: false doesn't return errors" do
    authors = create_authors()

    result =
      authors
      |> Ash.bulk_destroy(:destroy_manual_ok, %{},
        return_notifications?: true,
        return_errors?: false,
        return_records?: false,
        strategy: :stream
      )

    assert result.records == nil
    assert result.notifications == []
    assert result.error_count == 0
    assert result.errors == nil
  end
end
