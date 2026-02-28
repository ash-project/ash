defmodule Ash.Test.Type.AutoTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

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
    end

    calculations do
      # Simple: auto type from a single attribute ref
      calculate :title_calc, :auto, expr(title) do
        public?(true)
      end

      # Auto type from string concatenation
      calculate :full_text, :auto, expr(title <> " " <> body) do
        public?(true)
      end

      # Auto type from boolean expression
      calculate :is_active, :auto, expr(active == true) do
        public?(true)
      end

      # Auto type from an integer attribute
      calculate :score_calc, :auto, expr(score) do
        public?(true)
      end
    end

    relationships do
      has_many :comments, Ash.Test.Type.AutoTypeTest.Comment do
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
      # Cross-resource: auto type from a related attribute
      calculate :post_title, :auto, expr(post.title) do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Ash.Test.Type.AutoTypeTest.Post do
        public?(true)
        attribute_writable? true
      end
    end
  end

  describe "auto type resolution - simple attribute refs" do
    test "resolves type from a string attribute" do
      calc = Ash.Resource.Info.calculation(Post, :title_calc)
      assert calc.type == Ash.Type.String
    end

    test "resolves type from an integer attribute" do
      calc = Ash.Resource.Info.calculation(Post, :score_calc)
      assert calc.type == Ash.Type.Integer
    end

    test "resolves type from string concatenation" do
      calc = Ash.Resource.Info.calculation(Post, :full_text)
      assert calc.type == Ash.Type.String
    end

    test "resolves type from boolean expression" do
      calc = Ash.Resource.Info.calculation(Post, :is_active)
      assert calc.type == Ash.Type.Boolean
    end
  end

  describe "auto type resolution - runtime behavior" do
    test "auto-typed string calculation returns correct values" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      loaded = Ash.load!(post, :title_calc)
      assert loaded.title_calc == "Hello"
    end

    test "auto-typed integer calculation returns correct values" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      loaded = Ash.load!(post, :score_calc)
      assert loaded.score_calc == 42
    end

    test "auto-typed string concat returns correct values" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      loaded = Ash.load!(post, :full_text)
      assert loaded.full_text == "Hello World"
    end

    test "auto-typed boolean expression returns correct values" do
      post = Ash.Seed.seed!(Post, %{title: "Hello", body: "World", score: 42, active: true})
      loaded = Ash.load!(post, :is_active)
      assert loaded.is_active == true
    end
  end

  describe "auto type resolution - cross-resource refs" do
    test "resolves type from a related resource attribute" do
      calc = Ash.Resource.Info.calculation(Comment, :post_title)
      assert calc.type == Ash.Type.String
    end

    test "cross-resource auto-typed calc returns correct values" do
      post = Ash.Seed.seed!(Post, %{title: "My Post", body: "Body", score: 1, active: true})
      comment = Ash.Seed.seed!(Comment, %{text: "Nice!", post_id: post.id})
      loaded = Ash.load!(comment, :post_title)
      assert loaded.post_title == "My Post"
    end
  end
end
