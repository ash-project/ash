# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.AutoTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

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
            constraints: [fields: [body: [type: :string]]],
            tag: :type,
            tag_value: :text
          ],
          link: [
            type: :map,
            constraints: [fields: [url: [type: :string]]],
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
      belongs_to :post, Ash.Test.Type.AutoTypeTest.Post do
        public?(true)
        attribute_writable? true
      end
    end
  end

  defmodule Post do
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
      attribute :title, :string, public?: true
      attribute :body, :string, public?: true
      attribute :score, :integer, public?: true
      attribute :rating, :float, public?: true
      attribute :active, :boolean, public?: true
      attribute :amount, :decimal, public?: true
      attribute :slug, :ci_string, public?: true
      attribute :published_at, :utc_datetime_usec, public?: true
      attribute :priority, Priority, public?: true
      attribute :metadata, Metadata, public?: true
      attribute :content, Content, public?: true
      attribute :short_title, WrappedString, public?: true
      attribute :all_metadata, {:array, Metadata}, public?: true
      attribute :all_priorities, {:array, Priority}, public?: true
      attribute :tags_list, {:array, :string}, public?: true
    end

    calculations do
      calculate :title_calc, :auto, expr(title) do
        public?(true)
      end

      calculate :score_calc, :auto, expr(score) do
        public?(true)
      end

      calculate :rating_calc, :auto, expr(rating) do
        public?(true)
      end

      calculate :active_calc, :auto, expr(active) do
        public?(true)
      end

      calculate :id_calc, :auto, expr(id) do
        public?(true)
      end

      calculate :amount_calc, :auto, expr(amount) do
        public?(true)
      end

      calculate :slug_calc, :auto, expr(slug) do
        public?(true)
      end

      calculate :published_at_calc, :auto, expr(published_at) do
        public?(true)
      end

      calculate :priority_calc, :auto, expr(priority) do
        public?(true)
      end

      calculate :metadata_calc, :auto, expr(metadata) do
        public?(true)
      end

      calculate :content_calc, :auto, expr(content) do
        public?(true)
      end

      calculate :short_title_calc, :auto, expr(short_title) do
        public?(true)
      end

      calculate :all_metadata_calc, :auto, expr(all_metadata) do
        public?(true)
      end

      calculate :all_priorities_calc, :auto, expr(all_priorities) do
        public?(true)
      end

      calculate :tags_list_calc, :auto, expr(tags_list) do
        public?(true)
      end

      calculate :full_text, :auto, expr(title <> " " <> body) do
        public?(true)
      end

      calculate :is_active, :auto, expr(active == true) do
        public?(true)
      end

      calculate :conditional_title,
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

      calculate :score_plus_one, :auto, expr(score + 1) do
        public?(true)
      end

      calculate :has_tags, :auto, expr(exists(tags, true)) do
        public?(true)
      end

      calculate :tag_count_calc, :auto, expr(count(tags)) do
        public?(true)
      end

      calculate :card, :auto, expr(%{title: title, score: score, active: active}) do
        public?(true)
      end
    end

    relationships do
      has_many :tags, Tag do
        public?(true)
        destination_attribute :post_id
      end
    end
  end

  defmodule Comment do
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
      attribute :text, :string, public?: true
      attribute :post_id, :uuid, public?: true
    end

    calculations do
      calculate :post_title, :auto, expr(post.title) do
        public?(true)
      end

      calculate :post_priority, :auto, expr(post.priority) do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        attribute_writable? true
      end
    end
  end

  describe "auto type resolution - simple attribute refs" do
    test "string" do
      assert Ash.Resource.Info.calculation(Post, :title_calc).type == Ash.Type.String
    end

    test "integer" do
      assert Ash.Resource.Info.calculation(Post, :score_calc).type == Ash.Type.Integer
    end

    test "float" do
      assert Ash.Resource.Info.calculation(Post, :rating_calc).type == Ash.Type.Float
    end

    test "boolean" do
      assert Ash.Resource.Info.calculation(Post, :active_calc).type == Ash.Type.Boolean
    end

    test "uuid" do
      assert Ash.Resource.Info.calculation(Post, :id_calc).type == Ash.Type.UUID
    end

    test "decimal" do
      assert Ash.Resource.Info.calculation(Post, :amount_calc).type == Ash.Type.Decimal
    end

    test "ci_string" do
      assert Ash.Resource.Info.calculation(Post, :slug_calc).type == Ash.Type.CiString
    end

    test "utc_datetime_usec" do
      assert Ash.Resource.Info.calculation(Post, :published_at_calc).type ==
               Ash.Type.UtcDatetimeUsec
    end
  end

  describe "auto type resolution - complex attribute refs" do
    test "enum" do
      assert Ash.Resource.Info.calculation(Post, :priority_calc).type == Priority
    end

    test "embedded resource" do
      assert Ash.Resource.Info.calculation(Post, :metadata_calc).type == Metadata
    end

    test "union NewType" do
      assert Ash.Resource.Info.calculation(Post, :content_calc).type == Content
    end

    test "NewType (subtype_of :string)" do
      assert Ash.Resource.Info.calculation(Post, :short_title_calc).type == WrappedString
    end
  end

  describe "auto type resolution - array attribute refs" do
    test "array of embedded resources" do
      assert Ash.Resource.Info.calculation(Post, :all_metadata_calc).type == {:array, Metadata}
    end

    test "array of enums" do
      assert Ash.Resource.Info.calculation(Post, :all_priorities_calc).type == {:array, Priority}
    end

    test "array of strings" do
      assert Ash.Resource.Info.calculation(Post, :tags_list_calc).type ==
               {:array, Ash.Type.String}
    end
  end

  describe "auto type resolution - expressions" do
    test "string concatenation" do
      assert Ash.Resource.Info.calculation(Post, :full_text).type == Ash.Type.String
    end

    test "boolean comparison" do
      assert Ash.Resource.Info.calculation(Post, :is_active).type == Ash.Type.Boolean
    end

    test "conditional if/else" do
      assert Ash.Resource.Info.calculation(Post, :conditional_title).type == Ash.Type.String
    end

    test "integer arithmetic" do
      assert Ash.Resource.Info.calculation(Post, :score_plus_one).type == Ash.Type.Integer
    end

    test "exists() -> boolean" do
      assert Ash.Resource.Info.calculation(Post, :has_tags).type == Ash.Type.Boolean
    end

    test "count() aggregate -> integer" do
      assert Ash.Resource.Info.calculation(Post, :tag_count_calc).type == Ash.Type.Integer
    end

    test "map literal resolves to Ash.Type.Map with field constraints" do
      calc = Ash.Resource.Info.calculation(Post, :card)
      assert calc.type == Ash.Type.Map

      fields = calc.constraints[:fields]
      assert Keyword.get(fields, :title)[:type] == Ash.Type.String
      assert Keyword.get(fields, :score)[:type] == Ash.Type.Integer
      assert Keyword.get(fields, :active)[:type] == Ash.Type.Boolean
    end
  end

  describe "auto type resolution - cross-resource refs" do
    test "string attribute via relationship" do
      assert Ash.Resource.Info.calculation(Comment, :post_title).type == Ash.Type.String
    end

    test "enum attribute via relationship" do
      assert Ash.Resource.Info.calculation(Comment, :post_priority).type == Priority
    end
  end

  describe "runtime - simple types" do
    test "string" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      assert Ash.load!(post, :title_calc).title_calc == "Hello"
    end

    test "integer" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      assert Ash.load!(post, :score_calc).score_calc == 42
    end

    test "float" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, rating: 4.5, active: true})
      assert Ash.load!(post, :rating_calc).rating_calc == 4.5
    end

    test "boolean" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true})
      assert Ash.load!(post, :active_calc).active_calc == true
    end

    test "uuid" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true})
      loaded = Ash.load!(post, :id_calc)
      assert loaded.id_calc == post.id
    end

    test "decimal" do
      post =
        Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true, amount: "99.5"})

      loaded = Ash.load!(post, :amount_calc)
      assert Decimal.equal?(loaded.amount_calc, Decimal.new("99.5"))
    end

    test "ci_string" do
      post =
        Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true, slug: "My-Slug"})

      loaded = Ash.load!(post, :slug_calc)
      assert Ash.CiString.value(loaded.slug_calc) == "My-Slug"
    end
  end

  describe "runtime - complex types" do
    test "enum" do
      post =
        Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true, priority: :high})

      assert Ash.load!(post, :priority_calc).priority_calc == :high
    end

    test "embedded resource" do
      meta = %{source: "web", version: 3, address: %{street: "1 Main", city: "NYC", zip: "10001"}}

      post =
        Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true, metadata: meta})

      loaded = Ash.load!(post, :metadata_calc)
      assert %Metadata{source: "web", version: 3} = loaded.metadata_calc
      assert %Address{city: "NYC"} = loaded.metadata_calc.address
    end

    test "union NewType" do
      post =
        Ash.Seed.seed!(Post, %{
          title: "T",
          body: "B",
          score: 1,
          active: true,
          content: %{type: :text, body: "hello"}
        })

      loaded = Ash.load!(post, :content_calc)
      assert %Ash.Union{type: :text, value: value} = loaded.content_calc
      assert value.body == "hello"
    end

    test "NewType (subtype_of :string)" do
      post =
        Ash.Seed.seed!(Post, %{
          title: "T",
          body: "B",
          score: 1,
          active: true,
          short_title: "brief"
        })

      assert Ash.load!(post, :short_title_calc).short_title_calc == "brief"
    end
  end

  describe "runtime - array types" do
    test "array of enums" do
      post =
        Ash.Seed.seed!(Post, %{
          title: "T",
          body: "B",
          score: 1,
          active: true,
          all_priorities: [:high, :low]
        })

      assert Ash.load!(post, :all_priorities_calc).all_priorities_calc == [:high, :low]
    end

    test "array of strings" do
      post =
        Ash.Seed.seed!(Post, %{
          title: "T",
          body: "B",
          score: 1,
          active: true,
          tags_list: ["elixir", "ash"]
        })

      assert Ash.load!(post, :tags_list_calc).tags_list_calc == ["elixir", "ash"]
    end
  end

  describe "runtime - expressions" do
    test "string concatenation" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      assert Ash.load!(post, :full_text).full_text == "Hello World"
    end

    test "boolean comparison" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true})
      assert Ash.load!(post, :is_active).is_active == true
    end

    test "conditional if/else - nil branch" do
      post = Ash.Seed.seed!(Post, %{title: "OnlyTitle", score: 1, active: true})
      assert Ash.load!(post, :conditional_title).conditional_title == "OnlyTitle"
    end

    test "conditional if/else - else branch" do
      post = Ash.Seed.seed!(Post, %{title: "Foo", body: "Bar", score: 1, active: true})
      assert Ash.load!(post, :conditional_title).conditional_title == "Foo: Bar"
    end

    test "integer arithmetic" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 10, active: true})
      assert Ash.load!(post, :score_plus_one).score_plus_one == 11
    end

    test "map literal returns correct value" do
      post = Ash.Seed.seed!(Post, %{title: "Map", body: "B", score: 99, active: false})
      loaded = Ash.load!(post, :card)
      assert loaded.card == %{title: "Map", score: 99, active: false}
    end
  end

  describe "runtime - cross-resource" do
    test "string attribute via relationship" do
      post = Ash.Seed.seed!(Post, %{title: "My Post", body: "B", score: 1, active: true})
      comment = Ash.Seed.seed!(Comment, %{text: "Nice!", post_id: post.id})
      assert Ash.load!(comment, :post_title).post_title == "My Post"
    end

    test "enum attribute via relationship" do
      post =
        Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 1, active: true, priority: :critical})

      comment = Ash.Seed.seed!(Comment, %{text: "Urgent!", post_id: post.id})
      assert Ash.load!(comment, :post_priority).post_priority == :critical
    end
  end
end
