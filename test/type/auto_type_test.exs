# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.AutoTypeTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  # ── Shared types ──────────────────────────────────────────────────────

  defmodule PlainStruct do
    @moduledoc false
    defstruct [:name, :value]
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

  # ── Resources ─────────────────────────────────────────────────────────

  defmodule Author do
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
      attribute :name, :string, public?: true
      attribute :age, :integer, public?: true
      attribute :rating, :float, public?: true
      attribute :active, :boolean, public?: true
    end
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
      attribute :score, :integer, public?: true
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
      attribute :author_id, :uuid, public?: true
    end

    calculations do
      # simple attribute refs
      calculate :title_calc, :auto, expr(title), public?: true
      calculate :score_calc, :auto, expr(score), public?: true
      calculate :rating_calc, :auto, expr(rating), public?: true
      calculate :active_calc, :auto, expr(active), public?: true
      calculate :id_calc, :auto, expr(id), public?: true
      calculate :amount_calc, :auto, expr(amount), public?: true
      calculate :slug_calc, :auto, expr(slug), public?: true
      calculate :published_at_calc, :auto, expr(published_at), public?: true

      # complex type refs
      calculate :priority_calc, :auto, expr(priority), public?: true
      calculate :metadata_calc, :auto, expr(metadata), public?: true
      calculate :content_calc, :auto, expr(content), public?: true
      calculate :short_title_calc, :auto, expr(short_title), public?: true

      # array type refs
      calculate :all_metadata_calc, :auto, expr(all_metadata), public?: true
      calculate :all_priorities_calc, :auto, expr(all_priorities), public?: true
      calculate :tags_list_calc, :auto, expr(tags_list), public?: true

      # expressions
      calculate :full_text, :auto, expr(title <> " " <> body), public?: true
      calculate :is_active, :auto, expr(active == true), public?: true

      calculate :conditional_title,
                :auto,
                expr(
                  if is_nil(body) do
                    title
                  else
                    title <> ": " <> body
                  end
                ),
                public?: true

      calculate :score_plus_one, :auto, expr(score + 1), public?: true

      # type() wrapper
      calculate :typed_score, :auto, expr(type(score, :integer)), public?: true

      # aggregates
      calculate :has_tags, :auto, expr(exists(tags, true)), public?: true
      calculate :tag_count, :auto, expr(count(tags)), public?: true
      calculate :first_tag_label, :auto, expr(first(tags, field: :label)), public?: true
      calculate :max_tag_score, :auto, expr(max(tags, field: :score)), public?: true
      calculate :min_tag_score, :auto, expr(min(tags, field: :score)), public?: true
      calculate :sum_tag_score, :auto, expr(sum(tags, field: :score)), public?: true
      calculate :tag_labels, :auto, expr(list(tags, field: :label)), public?: true

      # map literal
      calculate :card, :auto, expr(%{title: title, score: score, active: active}), public?: true

      # struct literal (plain struct - not an Ash.Type)
      calculate :plain_struct_calc, :auto, expr(%PlainStruct{name: title, value: score}),
        public?: true

      # struct literal (Ash.Type - embedded resource)
      calculate :address_struct_calc, :auto, expr(%Address{street: title, city: body, zip: "00000"}),
        public?: true

      # cross-resource ref
      calculate :author_name, :auto, expr(author.name), public?: true

      # auto calc referencing another auto calc (dependency chain)
      calculate :score_calc_copy, :auto, expr(score_calc), public?: true
    end

    relationships do
      has_many :tags, Tag do
        public?(true)
        destination_attribute :post_id
      end

      belongs_to :author, Author do
        public?(true)
        attribute_writable? true
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
      calculate :post_title, :auto, expr(post.title), public?: true
      calculate :post_priority, :auto, expr(post.priority), public?: true
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        attribute_writable? true
      end
    end
  end

  # ── Type resolution tests ─────────────────────────────────────────────

  describe "simple attribute refs" do
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

  describe "complex type refs" do
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

  describe "array type refs" do
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

  describe "expression type resolution" do
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

    test "type() wrapper" do
      assert Ash.Resource.Info.calculation(Post, :typed_score).type == Ash.Type.Integer
    end

    test "map literal resolves to Ash.Type.Map with field constraints" do
      calc = Ash.Resource.Info.calculation(Post, :card)
      assert calc.type == Ash.Type.Map

      fields = calc.constraints[:fields]
      assert Keyword.get(fields, :title)[:type] == Ash.Type.String
      assert Keyword.get(fields, :score)[:type] == Ash.Type.Integer
      assert Keyword.get(fields, :active)[:type] == Ash.Type.Boolean
    end

    test "plain struct literal resolves to Ash.Type.Struct with instance_of" do
      calc = Ash.Resource.Info.calculation(Post, :plain_struct_calc)
      assert calc.type == Ash.Type.Struct
      assert calc.constraints[:instance_of] == PlainStruct
    end

    test "Ash.Type struct literal resolves to the type directly" do
      calc = Ash.Resource.Info.calculation(Post, :address_struct_calc)
      assert calc.type == Address
    end
  end

  describe "aggregate type resolution" do
    test "exists -> boolean" do
      assert Ash.Resource.Info.calculation(Post, :has_tags).type == Ash.Type.Boolean
    end

    test "count -> integer" do
      assert Ash.Resource.Info.calculation(Post, :tag_count).type == Ash.Type.Integer
    end

    test "first -> field type" do
      assert Ash.Resource.Info.calculation(Post, :first_tag_label).type == Ash.Type.String
    end

    test "max -> field type" do
      assert Ash.Resource.Info.calculation(Post, :max_tag_score).type == Ash.Type.Integer
    end

    test "min -> field type" do
      assert Ash.Resource.Info.calculation(Post, :min_tag_score).type == Ash.Type.Integer
    end

    test "sum -> field type" do
      assert Ash.Resource.Info.calculation(Post, :sum_tag_score).type == Ash.Type.Integer
    end

    test "list -> array of field type" do
      assert Ash.Resource.Info.calculation(Post, :tag_labels).type == {:array, Ash.Type.String}
    end
  end

  describe "determine_type for aggregate expressions" do
    test "Ash.Query.Aggregate struct with type set" do
      agg = %Ash.Query.Aggregate{
        name: :test_count,
        kind: :count,
        type: Ash.Type.Integer,
        constraints: [],
        relationship_path: [:tags],
        resource: Post
      }

      assert {:ok, {Ash.Type.Integer, []}} = Ash.Expr.determine_type(agg)
    end

    test "Ash.Query.Aggregate struct with nil type falls back to kind" do
      agg = %Ash.Query.Aggregate{
        name: :test_count,
        kind: :count,
        type: nil,
        constraints: [],
        relationship_path: [:tags],
        resource: Post
      }

      assert {:ok, {Ash.Type.Integer, []}} = Ash.Expr.determine_type(agg)
    end

    test "Ash.Query.Aggregate struct for exists with nil type" do
      agg = %Ash.Query.Aggregate{
        name: :test_exists,
        kind: :exists,
        type: nil,
        constraints: [],
        relationship_path: [:tags],
        resource: Post
      }

      assert {:ok, {Ash.Type.Boolean, []}} = Ash.Expr.determine_type(agg)
    end

    test "Ash.Query.Call for count aggregate" do
      call = %Ash.Query.Call{name: :count, args: [:tags]}
      assert {:ok, {Ash.Type.Integer, []}} = Ash.Expr.determine_type(call)
    end

    test "Ash.Query.Call for exists aggregate" do
      call = %Ash.Query.Call{name: :exists, args: [:tags, true]}
      assert {:ok, {Ash.Type.Boolean, []}} = Ash.Expr.determine_type(call)
    end

    test "Ash.Query.Call for avg aggregate" do
      call = %Ash.Query.Call{name: :avg, args: [:tags, [field: :score]]}
      assert {:ok, {Ash.Type.Float, []}} = Ash.Expr.determine_type(call)
    end
  end

  describe "cross-resource refs" do
    test "string attribute via relationship" do
      assert Ash.Resource.Info.calculation(Comment, :post_title).type == Ash.Type.String
    end

    test "enum attribute via relationship" do
      assert Ash.Resource.Info.calculation(Comment, :post_priority).type == Priority
    end

    test "ref through belongs_to" do
      assert Ash.Resource.Info.calculation(Post, :author_name).type == Ash.Type.String
    end
  end

  describe "dependency chain" do
    test "auto calc referencing another auto calc resolves" do
      assert Ash.Resource.Info.calculation(Post, :score_calc_copy).type == Ash.Type.Integer
    end
  end

  # ── Runtime value tests ───────────────────────────────────────────────

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
      assert Ash.load!(post, :id_calc).id_calc == post.id
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
        Ash.Seed.seed!(Post, %{
          title: "T",
          body: "B",
          score: 1,
          active: true,
          priority: :critical
        })

      comment = Ash.Seed.seed!(Comment, %{text: "Urgent!", post_id: post.id})
      assert Ash.load!(comment, :post_priority).post_priority == :critical
    end

    test "ref through belongs_to" do
      author = Ash.Seed.seed!(Author, %{name: "Alice", age: 30, rating: 4.5, active: true})
      post = Ash.Seed.seed!(Post, %{title: "Test", score: 1, active: true, author_id: author.id})
      assert Ash.load!(post, :author_name).author_name == "Alice"
    end
  end

  describe "runtime - dependency chain" do
    test "auto calc referencing another auto calc" do
      post = Ash.Seed.seed!(Post, %{title: "T", body: "B", score: 7, active: true})
      assert Ash.load!(post, :score_calc_copy).score_calc_copy == 7
    end
  end

  # ── Error tests ───────────────────────────────────────────────────────

  describe "errors" do
    test "auto type with non-expression calculation raises" do
      assert_raise Spark.Error.DslError,
                   ~r/`:auto` type is only supported for expression calculations/,
                   fn ->
                     defmodule BadCalcResource do
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
                       end

                       calculations do
                         calculate :bad_calc, :auto, fn records, _ ->
                           Enum.map(records, fn _ -> "nope" end)
                         end
                       end
                     end
                   end
    end
  end

  # ── TypeResolver GenServer tests ──────────────────────────────────────

  describe "TypeResolver GenServer" do
    alias Ash.TypeResolver

    setup do
      TypeResolver.shutdown()
      :ok
    end

    test "ensure_started is idempotent" do
      assert :ok = TypeResolver.ensure_started()
      assert :ok = TypeResolver.ensure_started()
      TypeResolver.shutdown()
    end

    test "resolves immediately when type is already known" do
      TypeResolver.ensure_started()
      TypeResolver.register_known_field(MyResource, :title, Ash.Type.String, [])
      assert {:ok, Ash.Type.String, []} = TypeResolver.resolve(MyResource, :title)
      TypeResolver.shutdown()
    end

    test "deadlock detection finds cycles" do
      TypeResolver.ensure_started()

      fake_calc_a = %Ash.Resource.Calculation{
        name: :calc_a,
        type: :auto,
        calculation: {Ash.Resource.Calculation.Expression, expr: nil}
      }

      fake_calc_b = %Ash.Resource.Calculation{
        name: :calc_b,
        type: :auto,
        calculation: {Ash.Resource.Calculation.Expression, expr: nil}
      }

      TypeResolver.register_auto(ResourceA, :calc_a, fake_calc_a, %{}, [{ResourceB, :calc_b}])
      TypeResolver.register_auto(ResourceB, :calc_b, fake_calc_b, %{}, [{ResourceA, :calc_a}])

      TypeResolver.done_registering(ResourceA)
      TypeResolver.done_registering(ResourceB)

      result = TypeResolver.resolve(ResourceA, :calc_a, 5_000)
      assert {:error, message} = result
      assert message =~ "Circular dependency"

      TypeResolver.shutdown()
    end
  end
end
