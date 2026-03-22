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

  defmodule Priority do
    @moduledoc false
    use Ash.Type.Enum, values: [:low, :medium, :high, :critical]
  end

  defmodule Address do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :street, :string, public?: true
      attribute :city, :string, public?: true
      attribute :zip, :string, public?: true
    end
  end

  defmodule Metadata do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :source, :string, public?: true
      attribute :version, :integer, public?: true
      attribute :address, Address, public?: true
    end
  end

  defmodule Content do
    @moduledoc false
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        types: [
          text: [
            type: :map,
            constraints: [
              fields: [
                body: [type: :string]
              ]
            ],
            tag: :type,
            tag_value: :text
          ],
          link: [
            type: :map,
            constraints: [
              fields: [
                url: [type: :string]
              ]
            ],
            tag: :type,
            tag_value: :link
          ]
        ]
      ]
  end

  defmodule WrappedString do
    @moduledoc false
    use Ash.Type.NewType, subtype_of: :string, constraints: [max_length: 255]
  end

  defmodule Tag do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :label, :string, public?: true
      attribute :post_id, :uuid, public?: true
    end

    relationships do
      belongs_to :post, Ash.Test.Type.AutoTypePubSubTest.Post do
        public? true
        define_attribute? false
        source_attribute :post_id
      end
    end
  end

  defmodule Post do
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
      publish :create, "created_with_tag_count", transform: :tag_count, event: "created_tag_count"

      publish :create, "created_with_metadata",
        transform: :metadata_list,
        event: "created_metadata"

      publish :create, "created_explicit_returns",
        transform: :display_name,
        returns: :integer,
        event: "created_explicit"

      publish :create, "created_with_card", transform: :card, event: "created_card"
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :body, :string, public?: true
      attribute :score, :decimal, public?: true, default: Decimal.new(0)
      attribute :view_count, :integer, public?: true, default: 0
      attribute :like_count, :integer, public?: true, default: 0
      attribute :active, :boolean, public?: true, default: true
      attribute :published_at, :utc_datetime_usec, public?: true
    end

    relationships do
      has_many :tags, Tag do
        public? true
      end
    end

    calculations do
      calculate :display_name, :auto, expr(title <> " - " <> body) do
        public?(true)
      end

      calculate :summary,
                :auto,
                expr(
                  if is_nil(body) do
                    title
                  else
                    title <> ": " <> body
                  end
                ) do
        public?(true)
      end

      calculate :is_active, :auto, expr(active and not is_nil(title)) do
        public?(true)
      end

      calculate :total_score, :auto, expr(view_count + like_count) do
        public?(true)
      end

      calculate :tag_count, :auto, expr(count(tags)) do
        public?(true)
      end

      calculate :metadata_list, {:array, :string}, expr(fragment("?", title)) do
        public?(true)
      end

      calculate :card, :auto, expr(%{title: title, view_count: view_count, active: active}) do
        public?(true)
      end
    end
  end

  defmodule ComplexPost do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Ash.Notifier.PubSub]

    ets do
      private?(true)
    end

    pub_sub do
      module PubSubModule
      prefix "complex"

      publish :create, "enum", transform: :current_priority, event: "enum"
      publish :create, "embedded", transform: :current_metadata, event: "embedded"
      publish :create, "nested_embedded", transform: :current_address, event: "nested_embedded"
      publish :create, "union", transform: :current_content, event: "union"
      publish :create, "newtype", transform: :wrapped_title, event: "newtype"
      publish :create, "array_embedded", transform: :metadata_copies, event: "array_embedded"
      publish :create, "array_enum", transform: :priority_list, event: "array_enum"
      publish :create, "decimal", transform: :current_score, event: "decimal"
      publish :create, "uuid", transform: :current_id, event: "uuid"
      publish :create, "datetime", transform: :current_published_at, event: "datetime"
      publish :create, "ci_string", transform: :current_slug, event: "ci_string"
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :slug, :ci_string, public?: true
      attribute :score, :decimal, public?: true
      attribute :published_at, :utc_datetime_usec, public?: true
      attribute :priority, Priority, public?: true
      attribute :metadata, Metadata, public?: true
      attribute :content, Content, public?: true
      attribute :short_title, WrappedString, public?: true
      attribute :all_metadata, {:array, Metadata}, public?: true
      attribute :all_priorities, {:array, Priority}, public?: true
    end

    calculations do
      calculate :current_priority, :auto, expr(priority) do
        public?(true)
      end

      calculate :current_metadata, :auto, expr(metadata) do
        public?(true)
      end

      # get_path can't auto-resolve; keep explicit type
      calculate :current_address, Address, expr(metadata[:address]) do
        public?(true)
      end

      calculate :current_content, :auto, expr(content) do
        public?(true)
      end

      calculate :wrapped_title, :auto, expr(short_title) do
        public?(true)
      end

      calculate :metadata_copies, :auto, expr(all_metadata) do
        public?(true)
      end

      calculate :priority_list, :auto, expr(all_priorities) do
        public?(true)
      end

      calculate :current_score, :auto, expr(score) do
        public?(true)
      end

      calculate :current_id, :auto, expr(id) do
        public?(true)
      end

      calculate :current_published_at, :auto, expr(published_at) do
        public?(true)
      end

      calculate :current_slug, :auto, expr(slug) do
        public?(true)
      end
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

  describe "type inference from calculations" do
    test "string concat infers :string" do
      pubs = publications_by_topic(Post)
      assert pubs[["created"]].returns == Ash.Type.String
    end

    test "conditional expr infers :string" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_with_summary"]].returns == Ash.Type.String
    end

    test "boolean expr infers :boolean" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_with_active"]].returns == Ash.Type.Boolean
    end

    test "integer arithmetic infers :integer" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_with_score"]].returns == Ash.Type.Integer
    end

    test "aggregate count infers :integer" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_with_tag_count"]].returns == Ash.Type.Integer
    end

    test "array type infers {:array, :string}" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_with_metadata"]].returns == {:array, Ash.Type.String}
    end

    test "map type infers :map with field constraints" do
      pubs = publications_by_topic(Post)
      pub = pubs[["created_with_card"]]
      assert pub.returns == Ash.Type.Map

      fields = pub.constraints[:fields]
      assert is_list(fields)
      assert Keyword.get(fields, :title)[:type] == Ash.Type.String
      assert Keyword.get(fields, :view_count)[:type] == Ash.Type.Integer
      assert Keyword.get(fields, :active)[:type] == Ash.Type.Boolean
    end

    test "explicit returns is not overridden" do
      pubs = publications_by_topic(Post)
      assert pubs[["created_explicit_returns"]].returns == :integer
    end
  end

  describe "complex type inference" do
    test "enum type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["enum"]].returns == Priority
    end

    test "embedded resource type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["embedded"]].returns == Metadata
    end

    test "nested embedded resource type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["nested_embedded"]].returns == Address
    end

    test "union (NewType) type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["union"]].returns == Content
    end

    test "NewType (subtype_of :string) is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["newtype"]].returns == WrappedString
    end

    test "array of embedded resources is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["array_embedded"]].returns == {:array, Metadata}
    end

    test "array of enums is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["array_enum"]].returns == {:array, Priority}
    end

    test "decimal type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["decimal"]].returns == Ash.Type.Decimal
    end

    test "uuid type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["uuid"]].returns == Ash.Type.UUID
    end

    test "utc_datetime_usec type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["datetime"]].returns == Ash.Type.UtcDatetimeUsec
    end

    test "ci_string type is propagated" do
      pubs = publications_by_topic(ComplexPost)
      assert pubs[["ci_string"]].returns == Ash.Type.CiString
    end
  end

  describe "complex type broadcasts" do
    test "enum calculation broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{title: "Urgent", priority: :critical})
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:enum", "enum", :critical}
    end

    test "embedded resource broadcasts correctly" do
      meta = %{
        source: "web",
        version: 2,
        address: %{street: "123 Main", city: "NYC", zip: "10001"}
      }

      ComplexPost
      |> Ash.Changeset.for_create(:create, %{title: "With Meta", metadata: meta})
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:embedded", "embedded", value}
      assert %Metadata{source: "web", version: 2} = value
      assert %Address{street: "123 Main", city: "NYC"} = value.address
    end

    test "union calculation broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{
        title: "With Content",
        content: %{type: :text, body: "hello"}
      })
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:union", "union", %Ash.Union{type: :text, value: value}}
      assert value.body == "hello"
    end

    test "NewType string broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{title: "Short", short_title: "brief"})
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:newtype", "newtype", "brief"}
    end

    test "decimal broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{title: "Scored", score: "99.5"})
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:decimal", "decimal", value}
      assert %Decimal{} = value
      assert Decimal.equal?(value, Decimal.new("99.5"))
    end

    test "ci_string broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{title: "Slugged", slug: "My-Slug"})
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:ci_string", "ci_string", value}
      assert Ash.CiString.value(value) == "My-Slug"
    end

    test "array of enums broadcasts correctly" do
      ComplexPost
      |> Ash.Changeset.for_create(:create, %{
        title: "Multi Priority",
        all_priorities: [:high, :low]
      })
      |> Ash.create!()

      assert_receive {:pub_sub, "complex:array_enum", "array_enum", [:high, :low]}
    end
  end

  describe "broadcast values" do
    test "string calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Hello", body: "World"})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created", "create", "Hello - World"}
    end

    test "conditional calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Title Only"})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_summary", "created_summary", "Title Only"}
    end

    test "conditional calculation with both branches" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Foo", body: "Bar"})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_summary", "created_summary", "Foo: Bar"}
    end

    test "boolean calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Active Post", active: true})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_active", "created_active", true}
    end

    test "integer calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "Scored",
        view_count: 5,
        like_count: 3
      })
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_score", "created_score", 8}
    end

    test "aggregate calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Tagged"})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_tag_count", "created_tag_count", 0}
    end

    test "map calculation broadcasts correctly" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Map Post", view_count: 42, active: true})
      |> Ash.create!()

      assert_receive {:pub_sub, "post:created_with_card", "created_card", value}
      assert %{title: "Map Post", view_count: 42, active: true} = value
    end
  end
end
