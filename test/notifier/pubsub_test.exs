defmodule Ash.Test.Notifier.PubSubTest do
  @moduledoc false
  use ExUnit.Case, async: false

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
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Ash.Notifier.PubSub
      ]

    pub_sub do
      module PubSub
      prefix "post"

      publish :destroy, ["foo", :id]
      publish :default, ["foo", :id], type: :update
      publish :default, ["bar", :name], type: :update, event: "name_change"
    end

    ets do
      private?(true)
    end

    actions do
      create :default
      read :default
      update :default
      destroy :destroy
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0

      attribute :name, :string
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource Post
    end
  end

  setup do
    Application.put_env(PubSub, :notifier_test_pid, self())

    :ok
  end

  test "publishing a message with a change value" do
    post =
      Post
      |> Ash.Changeset.new(%{})
      |> Api.create!()

    Api.destroy!(post)

    message = "post:foo:#{post.id}"
    assert_receive {:broadcast, ^message, "destroy", %Ash.Notifier.Notification{}}
  end

  test "publishing a message with multiple matches/changes" do
    post =
      Post
      |> Ash.Changeset.new(%{name: "ted"})
      |> Api.create!()

    post
    |> Ash.Changeset.new(%{name: "joe"})
    |> Api.update!()

    message = "post:foo:#{post.id}"
    assert_receive {:broadcast, ^message, "default", %Ash.Notifier.Notification{}}

    message = "post:bar:joe"
    assert_receive {:broadcast, ^message, "name_change", %Ash.Notifier.Notification{}}
    message = "post:bar:ted"
    assert_receive {:broadcast, ^message, "name_change", %Ash.Notifier.Notification{}}
  end
end
