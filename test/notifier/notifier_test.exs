defmodule Ash.Test.NotifierTest do
  @moduledoc false
  use ExUnit.Case, async: false

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
    end
  end

  defmodule PostLink do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Notifier
      ]

    ets do
      private? true
    end

    actions do
      create :default
      read :default
      update :default
      destroy :default
    end

    relationships do
      belongs_to :source_post, Ash.Test.NotifierTest.Post,
        primary_key?: true,
        required?: true

      belongs_to :destination_post, Ash.Test.NotifierTest.Post,
        primary_key?: true,
        required?: true
    end
  end

  defmodule Comment do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Notifier
      ]

    ets do
      private?(true)
    end

    actions do
      create :default
      read :default
      update :default
      destroy :default
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    relationships do
      belongs_to :post, Ash.Test.NotifierTest.Post
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Notifier
      ]

    ets do
      private?(true)
    end

    actions do
      create :default
      read :default
      update :default
      destroy :default
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string
    end

    relationships do
      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id

      has_many :comments, Comment, destination_field: :post_id
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource Post
      resource PostLink
      resource Comment
    end
  end

  setup do
    Application.put_env(Notifier, :notifier_test_pid, self())

    :ok
  end

  describe "simple creates and updates" do
    test "a create notification occurs" do
      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Api.create!()

      assert_receive {:notification, %{action: %{type: :create}}}
    end

    test "an update notification occurs" do
      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Api.create!()
      |> Ash.Changeset.new(%{name: "bar"})
      |> Api.update!()

      assert_receive {:notification, %{action: %{type: :update}}}
    end

    test "a destroy notification occurs" do
      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Api.create!()
      |> Api.destroy!()

      assert_receive {:notification, %{action: %{type: :destroy}}}
    end
  end

  describe "related notifications" do
    test "an update notification occurs when relating many to many" do
      comment =
        Comment
        |> Ash.Changeset.new(%{})
        |> Api.create!()

      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Ash.Changeset.replace_relationship(:comments, comment)
      |> Api.create!()

      assert_receive {:notification, %{action: %{type: :update}, resource: Comment}}
    end

    test "a create notification occurs for the join through relationship" do
      post =
        Post
        |> Ash.Changeset.new(%{name: "foo"})
        |> Api.create!()

      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Ash.Changeset.replace_relationship(:related_posts, [post])
      |> Api.create!()

      assert_receive {:notification, %{action: %{type: :create}, resource: PostLink}}
    end

    test "a destroy notification occurs for the join through relationship" do
      post =
        Post
        |> Ash.Changeset.new(%{name: "foo"})
        |> Api.create!()

      Post
      |> Ash.Changeset.new(%{name: "foo"})
      |> Ash.Changeset.replace_relationship(:related_posts, [post])
      |> Api.create!()
      |> Ash.Changeset.new(%{})
      |> Ash.Changeset.replace_relationship(:related_posts, [])
      |> Api.update!()

      assert_receive {:notification, %{action: %{type: :destroy}, resource: PostLink}}
    end
  end
end
