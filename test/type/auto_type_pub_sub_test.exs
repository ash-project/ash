# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.AutoTypePubSubTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule PubSubModule do
    @moduledoc false
    def broadcast(topic, event, payload) do
      send(
        Application.get_env(__MODULE__, :test_pid),
        {:pub_sub, topic, event, payload}
      )
    end
  end

  defmodule PubSubPost do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Ash.Notifier.PubSub]

    ets do
      private?(true)
    end

    pub_sub do
      module PubSubModule
      prefix "post"

      publish :create, "created", transform: :display_name
      publish :create, "created_with_summary", transform: :summary, event: "created_summary"
      publish :create, "created_with_active", transform: :is_active, event: "created_active"
      publish :create, "created_with_score", transform: :total_score, event: "created_score"

      publish :create, "created_explicit_returns",
        transform: :display_name,
        returns: :integer,
        event: "created_explicit"
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :body, :string, public?: true
      attribute :view_count, :integer, public?: true, default: 0
      attribute :like_count, :integer, public?: true, default: 0
      attribute :active, :boolean, public?: true, default: true
    end

    calculations do
      calculate :display_name, :auto, expr(title <> " - " <> body), public?: true

      calculate :summary,
                :auto,
                expr(
                  if is_nil(body) do
                    title
                  else
                    title <> ": " <> body
                  end
                ),
                public?: true

      calculate :is_active, :auto, expr(active and not is_nil(title)), public?: true
      calculate :total_score, :auto, expr(view_count + like_count), public?: true
    end
  end

  setup do
    Application.put_env(PubSubModule, :test_pid, self())
    :ok
  end

  defp publications_by_topic(resource) do
    resource
    |> Ash.Notifier.PubSub.Info.publications()
    |> Map.new(fn pub -> {pub.topic, pub} end)
  end

  describe "type inference" do
    test "string concat infers :string" do
      pubs = publications_by_topic(PubSubPost)
      assert pubs[["created"]].returns == Ash.Type.String
    end

    test "conditional expr infers :string" do
      pubs = publications_by_topic(PubSubPost)
      assert pubs[["created_with_summary"]].returns == Ash.Type.String
    end

    test "boolean expr infers :boolean" do
      pubs = publications_by_topic(PubSubPost)
      assert pubs[["created_with_active"]].returns == Ash.Type.Boolean
    end

    test "integer arithmetic infers :integer" do
      pubs = publications_by_topic(PubSubPost)
      assert pubs[["created_with_score"]].returns == Ash.Type.Integer
    end

    test "explicit returns is not overridden" do
      pubs = publications_by_topic(PubSubPost)
      assert pubs[["created_explicit_returns"]].returns == :integer
    end
  end

  describe "broadcast values" do
    test "string calculation broadcasts correctly" do
      PubSubPost
      |> Ash.Changeset.for_create(:create, %{title: "Hello", body: "World"})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created", "create", "Hello - World"}
    end

    test "integer calculation broadcasts correctly" do
      PubSubPost
      |> Ash.Changeset.for_create(:create, %{title: "S", body: "B", view_count: 5, like_count: 3})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_score", "created_score", 8}
    end
  end
end
