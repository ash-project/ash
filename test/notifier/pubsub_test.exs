# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Notifier.PubSubTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule PubSub do
    def broadcast(topic, event, notification) do
      send(
        Application.get_env(__MODULE__, :notifier_test_pid),
        {:broadcast, topic, event, notification}
      )
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Ash.Notifier.PubSub
      ]

    pub_sub do
      module PubSub
      prefix "post"

      publish :destroy, ["foo", :id]
      publish :update, ["foo", :id], previous_values?: true
      publish :update, ["bar", :name], event: "name_change", previous_values?: true
      publish :update_pkey, ["foo", :_pkey], previous_values?: true

      publish :update_with_transform, ["update_with_transform", :name] do
        transform fn notification ->
          notification.data
        end

        filter fn notification ->
          notification.data.name == "george"
        end
      end

      publish_all :update, ["baz", :id], event: "any_update", except: [:update_pkey]
      publish_all :update, ["fiz", :id], event: "any_update", except: [:doesnotexist]
    end

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :destroy]

      create :create do
        accept :*
      end

      update :update do
        require_atomic? false
        accept :*
      end

      update :update_with_transform do
        require_atomic? false
        accept :*
      end

      update :update_pkey do
        accept :*
        require_atomic? false
      end
    end

    attributes do
      uuid_primary_key :id, writable?: true

      attribute :name, :string do
        public?(true)
      end
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Ash.Notifier.PubSub
      ]

    pub_sub do
      module PubSub
      prefix "users"
      delimiter "."

      publish :create, [:id, "created"]
    end

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults create: :*
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  defmodule PostWithLoad do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Ash.Notifier.PubSub]

    pub_sub do
      module PubSub
      prefix "post_load"

      publish("named_create", :create, "created", load: [:name])
      publish :update, "updated", load: [:name]
      publish_all("any_create", :create, "any_created", load: [:name])
    end

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  setup do
    Application.put_env(PubSub, :notifier_test_pid, self())

    :ok
  end

  test "publishing a message with a change value" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    Ash.destroy!(post)

    message = "post:foo:#{post.id}"
    assert_receive {:broadcast, ^message, "destroy", %Ash.Notifier.Notification{}}
  end

  test "from is the pid that sent the message" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    Ash.destroy!(post)

    message = "post:foo:#{post.id}"
    pid = self()
    assert_receive {:broadcast, ^message, "destroy", %Ash.Notifier.Notification{from: ^pid}}
  end

  test "notification_metadata is included" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    Ash.destroy!(post, notification_metadata: %{foo: :bar})

    message = "post:foo:#{post.id}"

    assert_receive {:broadcast, ^message, "destroy",
                    %Ash.Notifier.Notification{metadata: %{foo: :bar}}}
  end

  test "publishing with a filter and a transform" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{name: "ted"})
      |> Ash.create!()

    post
    |> Ash.Changeset.for_update(:update_with_transform, %{name: "george"})
    |> Ash.update!()

    post
    |> Ash.Changeset.for_update(:update_with_transform, %{name: "fred"})
    |> Ash.update!()

    assert_receive {
      :broadcast,
      "post:update_with_transform:george",
      "update_with_transform",
      %Post{}
    }

    refute_receive {
      :broadcast,
      "post:update_with_transform:fred",
      "update_with_transform",
      %Post{}
    }
  end

  test "publishing a message with multiple matches/changes" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{name: "ted"})
      |> Ash.create!()

    post
    |> Ash.Changeset.for_update(:update, %{name: "joe"})
    |> Ash.update!()

    message = "post:foo:#{post.id}"
    assert_receive {:broadcast, ^message, "update", %Ash.Notifier.Notification{}}

    message = "post:bar:joe"
    assert_receive {:broadcast, ^message, "name_change", %Ash.Notifier.Notification{}}
    message = "post:bar:ted"
    assert_receive {:broadcast, ^message, "name_change", %Ash.Notifier.Notification{}}
    message = "post:baz:#{post.id}"
    assert_receive {:broadcast, ^message, "any_update", %Ash.Notifier.Notification{}}
    message = "post:fiz:#{post.id}"
    assert_receive {:broadcast, ^message, "any_update", %Ash.Notifier.Notification{}}
  end

  test "publishing a message with a pkey matcher" do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{name: "ted"})
      |> Ash.create!()

    new_id = Ash.UUID.generate()

    post
    |> Ash.Changeset.for_update(:update, %{id: new_id})
    |> Ash.update!(action: :update_pkey)

    message = "post:foo:#{post.id}"
    assert_receive {:broadcast, ^message, "update_pkey", %Ash.Notifier.Notification{}}

    message = "post:foo:#{new_id}"
    assert_receive {:broadcast, ^message, "update_pkey", %Ash.Notifier.Notification{}}
    message = "post:baz:#{new_id}"
    refute_receive {:broadcast, ^message, "any_update", %Ash.Notifier.Notification{}}
    message = "post:fiz:#{new_id}"
    assert_receive {:broadcast, ^message, "any_update", %Ash.Notifier.Notification{}}
  end

  test "publishing a message with a different delimiter" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{name: "Dave"})
      |> Ash.create!()

    message = "users.#{user.id}.created"
    assert_receive {:broadcast, ^message, "create", %Ash.Notifier.Notification{}}
  end

  describe "event as optional first positional argument" do
    test "atom event as first positional arg is used as the broadcast event" do
      PostWithLoad
      |> Ash.Changeset.for_create(:create, %{name: "alice"})
      |> Ash.create!()

      assert_receive {:broadcast, "post_load:created", "named_create",
                      %Ash.Notifier.Notification{}}
    end

    test "string event as first positional arg is used as the broadcast event" do
      PostWithLoad
      |> Ash.Changeset.for_create(:create, %{name: "bob"})
      |> Ash.create!()

      assert_receive {:broadcast, "post_load:any_created", "any_create",
                      %Ash.Notifier.Notification{}}
    end

    test "event set as keyword option still works" do
      Post
      |> Ash.Changeset.for_create(:create, %{name: "ted"})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{name: "joe"})
      |> Ash.update!()

      assert_receive {:broadcast, _, "name_change", %Ash.Notifier.Notification{}}
    end
  end

  describe "publication load" do
    test "loaded fields are available on notification.data during broadcast" do
      PostWithLoad
      |> Ash.Changeset.for_create(:create, %{name: "carol"})
      |> Ash.create!()

      assert_receive {:broadcast, "post_load:created", "named_create",
                      %Ash.Notifier.Notification{data: %{name: "carol"}}}
    end

    test "publish_all with load loads fields for notification" do
      PostWithLoad
      |> Ash.Changeset.for_create(:create, %{name: "dave"})
      |> Ash.create!()

      assert_receive {:broadcast, "post_load:any_created", "any_create",
                      %Ash.Notifier.Notification{data: %{name: "dave"}}}
    end
  end

  describe "public? option on publications" do
    test "public? defaults to false" do
      publications = Ash.Notifier.PubSub.Info.publications(Post)
      assert Enum.all?(publications, &(&1.public? == false))
    end

    test "public? can be set to true" do
      defmodule PublicPost do
        use Ash.Resource,
          domain: Ash.Test.Domain,
          data_layer: Ash.DataLayer.Ets,
          notifiers: [Ash.Notifier.PubSub]

        pub_sub do
          module Ash.Test.Notifier.PubSubTest.PubSub

          publish :create, "created", public?: true
          publish :update, "updated"
        end

        ets do
          private?(true)
        end

        actions do
          default_accept :*
          defaults [:read, create: :*, update: :*]
        end

        attributes do
          uuid_primary_key :id
        end
      end

      publications = Ash.Notifier.PubSub.Info.publications(PublicPost)
      create_pub = Enum.find(publications, &(&1.action == :create))
      update_pub = Enum.find(publications, &(&1.action == :update))
      assert create_pub.public? == true
      assert update_pub.public? == false
    end
  end

  describe "returns/transform typed publications" do
    test "transform option shapes the broadcast payload" do
      PostWithLoad
      |> Ash.Changeset.for_create(:create, %{name: "alice"})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{name: "bob"})
      |> Ash.update!()

      assert_receive {:broadcast, "post_load:updated", "update",
                      %Ash.Notifier.Notification{data: %{name: "bob"}}}
    end
  end
end
