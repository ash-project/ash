# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.NotifierTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
      :ok
    end
  end

  defmodule Notifier2 do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
      :ok
    end
  end

  defmodule LoadNotifier do
    use Ash.Notifier

    def load(_resource, _action), do: [:comments]

    def notify(notification) do
      send(
        Application.get_env(__MODULE__, :notifier_test_pid),
        {:load_notification, notification}
      )

      :ok
    end
  end

  # Both of these notifiers request the same :comments field — the dep resolver
  # should load it only once.
  defmodule ConflictingLoadNotifier1 do
    use Ash.Notifier

    def load(_resource, _action), do: [:comments]

    def notify(notification) do
      send(
        Application.get_env(__MODULE__, :notifier_test_pid),
        {:conflict_notifier_1, notification}
      )

      :ok
    end
  end

  defmodule ConflictingLoadNotifier2 do
    use Ash.Notifier

    def load(_resource, _action), do: [:comments]

    def notify(notification) do
      send(
        Application.get_env(__MODULE__, :notifier_test_pid),
        {:conflict_notifier_2, notification}
      )

      :ok
    end
  end

  defmodule PostLink do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      simple_notifiers: [
        Notifier
      ]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update do
        primary? true
        require_atomic? false
      end
    end

    relationships do
      belongs_to :source_post, Ash.Test.NotifierTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.NotifierTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Notifier
      ]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]

      update :update do
        primary? true
        require_atomic? false
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Ash.Test.NotifierTest.Post do
        public?(true)
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      simple_notifiers: [
        Notifier
      ]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      update :update do
        primary? true
        require_atomic? false
      end

      destroy :destroy do
        primary? true
        notifiers([Notifier2])
      end

      create :create_with_comment do
        change load(:comments)

        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            Comment
            |> Ash.Changeset.for_create(:create, %{post_id: result.id, name: "auto"})
            |> Ash.create!()

            {:ok, result}
          end)
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :related_posts, __MODULE__,
        public?: true,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id

      has_many :comments, Comment, destination_attribute: :post_id, public?: true
    end
  end

  defmodule PostWithConflictingLoadNotifiers do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [ConflictingLoadNotifier1, ConflictingLoadNotifier2]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :comments, Comment, destination_attribute: :post_id, public?: true
    end
  end

  defmodule PostWithLoadNotifier do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [LoadNotifier]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :comments, Comment, destination_attribute: :post_id, public?: true
    end
  end

  defmodule TransactionalPost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      notifiers: [Notifier]

    actions do
      default_accept :*
      defaults [:read, create: :*]

      create :create_with_nested do
        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            Ash.create!(__MODULE__, %{name: "inner"})

            {:ok, result}
          end)
        end
      end

      create :create_then_fail do
        change fn changeset, _ ->
          Ash.Changeset.after_action(changeset, fn _changeset, _result ->
            Ash.create!(__MODULE__, %{name: "inner"})

            {:error, "boom"}
          end)
        end
      end

      read :read_and_create do
        transaction? true

        prepare fn query, _ ->
          Ash.Query.before_action(query, fn query ->
            Ash.create!(__MODULE__, %{name: "inner"})
            query
          end)
        end
      end

      read :read_create_then_raise do
        transaction? true

        prepare fn query, _ ->
          Ash.Query.after_action(query, fn _query, _results ->
            Ash.create!(__MODULE__, %{name: "inner"})
            raise "boom"
          end)
        end
      end

      action :generic_create_then_fail do
        transaction? true

        run fn _input, _ ->
          Ash.create!(__MODULE__, %{name: "inner"})

          {:error, "boom"}
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  setup do
    Application.put_env(Notifier, :notifier_test_pid, self())
    Application.put_env(Notifier2, :notifier_test_pid, self())
    Application.put_env(LoadNotifier, :notifier_test_pid, self())
    Application.put_env(ConflictingLoadNotifier1, :notifier_test_pid, self())
    Application.put_env(ConflictingLoadNotifier2, :notifier_test_pid, self())

    :ok
  end

  describe "simple creates and updates" do
    test "a create notification occurs" do
      Post
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.create!()

      assert_receive {:notification, %{action: %{type: :create}}}
    end

    test "an update notification occurs" do
      Post
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{name: "bar"})
      |> Ash.update!()

      assert_receive {:notification, %{action: %{type: :update}}}
    end

    test "a destroy notification occurs" do
      Post
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.create!()
      |> Ash.destroy!()

      assert_receive {:notification, %{action: %{type: :destroy}}}
      assert_receive {:notification, %{action: %{type: :destroy}}}
    end
  end

  describe "custom notifications" do
    test "a custom notification can be returned in a before or after action hook" do
      Comment
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.Changeset.before_action(fn changeset ->
        {changeset,
         %{
           notifications: [
             %Ash.Notifier.Notification{
               resource: changeset.resource,
               domain: Ash.Resource.Info.domain(changeset.resource),
               metadata: %{custom?: true}
             }
           ]
         }}
      end)
      |> Ash.create!()

      assert_receive {:notification, %Ash.Notifier.Notification{metadata: %{custom?: true}}}
    end

    test "a custom notification without an action does not crash when telemetry handlers are attached" do
      handler_id = {__MODULE__, :notifier_telemetry, System.unique_integer()}
      test_pid = self()

      :telemetry.attach(
        handler_id,
        [:ash, :notifier, :start],
        fn _event, _measurements, metadata, _config ->
          send(test_pid, {:telemetry_metadata, metadata})
        end,
        nil
      )

      on_exit(fn -> :telemetry.detach(handler_id) end)

      Comment
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.Changeset.before_action(fn changeset ->
        {changeset,
         %{
           notifications: [
             %Ash.Notifier.Notification{
               resource: changeset.resource,
               domain: Ash.Resource.Info.domain(changeset.resource),
               metadata: %{custom?: true}
             }
           ]
         }}
      end)
      |> Ash.create!()

      assert_receive {:notification, %Ash.Notifier.Notification{metadata: %{custom?: true}}}
      assert_receive {:telemetry_metadata, %{action: nil, resource: Comment}}
    end
  end

  test "a nested notification is sent automatically" do
    Post
    |> Ash.Changeset.for_create(:create_with_comment, %{name: "foobar"})
    |> Ash.create!()

    assert_receive {:notification, %Ash.Notifier.Notification{data: %Comment{name: "auto"}}}
  end

  test "the `load/1` change puts the loaded data into the notification" do
    Post
    |> Ash.Changeset.for_create(:create_with_comment, %{name: "foobar"})
    |> Ash.create!()

    assert_receive {:notification, %Ash.Notifier.Notification{data: %Post{comments: [_]}}}
  end

  test "notifications use the data before its limited by a select statement" do
    Comment
    |> Ash.Changeset.for_create(:create, %{name: "foobar"})
    |> Ash.Changeset.select([:id])
    |> Ash.create!()

    assert_receive {:notification, %Ash.Notifier.Notification{data: %{name: "foobar"}}}
  end

  test "notifications use the changeset after before_action callbacks" do
    Comment
    |> Ash.Changeset.for_create(:create, %{name: "foobar"})
    |> Ash.Changeset.before_action(fn changeset ->
      Ash.Changeset.set_context(changeset, %{foobar: :baz})
    end)
    |> Ash.Changeset.select([:id])
    |> Ash.create!()

    assert_receive {:notification,
                    %Ash.Notifier.Notification{changeset: %{context: %{foobar: :baz}}}}
  end

  describe "load/2 callback" do
    test "loaded fields are available on notification.data" do
      PostWithLoadNotifier
      |> Ash.Changeset.for_create(:create, %{name: "test"})
      |> Ash.create!()

      assert_receive {:load_notification, %Ash.Notifier.Notification{data: %{comments: comments}}}

      assert comments == []
    end

    test "two notifiers requesting the same field both receive the loaded data" do
      PostWithConflictingLoadNotifiers
      |> Ash.Changeset.for_create(:create, %{name: "conflict"})
      |> Ash.create!()

      assert_receive {:conflict_notifier_1,
                      %Ash.Notifier.Notification{data: %{comments: comments1}}}

      assert_receive {:conflict_notifier_2,
                      %Ash.Notifier.Notification{data: %{comments: comments2}}}

      assert comments1 == []
      assert comments2 == []
    end
  end

  describe "related notifications" do
    test "an update notification occurs when relating many to many" do
      comment =
        Comment
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.Changeset.manage_relationship(:comments, comment, type: :append_and_remove)
      |> Ash.create!()

      assert_receive {:notification, %{action: %{type: :update}, resource: Comment}}
    end

    test "a create notification occurs for the join through relationship" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{name: "foo"})
      |> Ash.Changeset.manage_relationship(:related_posts, [post], type: :append_and_remove)
      |> Ash.create!()

      assert_receive {:notification, %{action: %{type: :create}, resource: PostLink}}
    end

    test "a destroy notification occurs for the join through relationship" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      assert %{related_posts: [_]} =
               post =
               Post
               |> Ash.Changeset.for_create(:create, %{name: "foo"})
               |> Ash.Changeset.manage_relationship(:related_posts, [post],
                 type: :append_and_remove
               )
               |> Ash.create!()
               |> Ash.load!(:related_posts)

      assert %{related_posts: []} =
               post
               |> Ash.Changeset.for_update(:update, %{})
               |> Ash.Changeset.manage_relationship(:related_posts, [], type: :append_and_remove)
               |> Ash.update!()

      assert_receive {:notification, %{action: %{type: :destroy}, resource: PostLink}}
    end
  end

  describe "rolled back transactions" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [TransactionalPost])
      end)

      on_exit(fn ->
        capture_log(fn ->
          :mnesia.stop()
          :mnesia.delete_schema([node()])
        end)
      end)
    end

    test "notifications queued in a rolled back create are not sent by the next action" do
      assert {:error, _} =
               TransactionalPost
               |> Ash.Changeset.for_create(:create_then_fail, %{name: "outer"})
               |> Ash.create()

      refute Process.get(:ash_notifications)

      Ash.create!(TransactionalPost, %{name: "next"})

      assert_receive {:notification, %{data: %TransactionalPost{name: "next"}}}
      refute_received {:notification, %{data: %TransactionalPost{name: "inner"}}}
      assert [%{name: "next"}] = Ash.read!(TransactionalPost)
    end

    test "notifications queued in a rolled back generic action are not sent by the next action" do
      assert {:error, _} =
               TransactionalPost
               |> Ash.ActionInput.for_action(:generic_create_then_fail, %{})
               |> Ash.run_action()

      refute Process.get(:ash_notifications)

      Ash.create!(TransactionalPost, %{name: "next"})

      assert_receive {:notification, %{data: %TransactionalPost{name: "next"}}}
      refute_received {:notification, %{data: %TransactionalPost{name: "inner"}}}
      assert [%{name: "next"}] = Ash.read!(TransactionalPost)
    end

    test "notifications queued in a committed read transaction are sent when it completes" do
      Ash.read!(TransactionalPost, action: :read_and_create)

      assert_receive {:notification, %{data: %TransactionalPost{name: "inner"}}}
      refute Process.get(:ash_notifications)
      refute Process.get(:ash_started_transaction?)
    end

    test "notifications queued in a rolled back read are not sent by the next action" do
      assert_raise Ash.Error.Unknown, fn ->
        Ash.read!(TransactionalPost, action: :read_create_then_raise)
      end

      refute Process.get(:ash_notifications)

      Ash.create!(TransactionalPost, %{name: "next"})

      assert_receive {:notification, %{data: %TransactionalPost{name: "next"}}}
      refute_received {:notification, %{data: %TransactionalPost{name: "inner"}}}
      assert [%{name: "next"}] = Ash.read!(TransactionalPost)
    end

    test "`return_notifications?: true` returns notifications queued by nested actions" do
      {:ok, _, notifications} =
        TransactionalPost
        |> Ash.Changeset.for_create(:create_with_nested, %{name: "outer"})
        |> Ash.create(return_notifications?: true)

      assert notifications |> Enum.map(& &1.data.name) |> Enum.sort() == ["inner", "outer"]
      refute Process.get(:ash_notifications)
      refute_received {:notification, _}
    end

    test "a bulk create with `transaction: :all` does not leave notifications queued for later" do
      Ash.bulk_create!([%{name: "bulk"}], TransactionalPost, :create,
        transaction: :all,
        notify?: true
      )

      assert_receive {:notification, %{data: %TransactionalPost{name: "bulk"}}}
      refute Process.get(:ash_started_transaction?)

      Ash.create!(TransactionalPost, %{name: "next"})

      assert_receive {:notification, %{data: %TransactionalPost{name: "next"}}}
    end
  end
end
