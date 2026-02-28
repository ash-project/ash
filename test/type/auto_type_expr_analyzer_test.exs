defmodule Ash.Test.Type.AutoTypeExprAnalyzerTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule TestDomain do
    @moduledoc false
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Author do
    use Ash.Resource,
      domain: TestDomain,
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

  defmodule Article do
    use Ash.Resource,
      domain: TestDomain,
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
      attribute :word_count, :integer, public?: true
      attribute :author_id, :uuid, public?: true
    end

    calculations do
      calculate :author_name, :auto, expr(author.name) do
        public?(true)
      end

      calculate :title_copy, :auto, expr(title) do
        public?(true)
      end

      calculate :word_count_copy, :auto, expr(word_count) do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
        attribute_writable? true
      end
    end
  end

  describe "type resolution for various expression forms" do
    test "resolves string attribute ref" do
      calc = Ash.Resource.Info.calculation(Article, :title_copy)
      assert calc.type == Ash.Type.String
    end

    test "resolves integer attribute ref" do
      calc = Ash.Resource.Info.calculation(Article, :word_count_copy)
      assert calc.type == Ash.Type.Integer
    end

    test "resolves cross-resource ref" do
      calc = Ash.Resource.Info.calculation(Article, :author_name)
      assert calc.type == Ash.Type.String
    end
  end

  describe "runtime values are correct" do
    test "string attribute ref returns correct value" do
      article = Ash.Seed.seed!(Article, %{title: "Test", word_count: 100})
      loaded = Ash.load!(article, :title_copy)
      assert loaded.title_copy == "Test"
    end

    test "integer attribute ref returns correct value" do
      article = Ash.Seed.seed!(Article, %{title: "Test", word_count: 100})
      loaded = Ash.load!(article, :word_count_copy)
      assert loaded.word_count_copy == 100
    end

    test "cross-resource ref returns correct value" do
      author = Ash.Seed.seed!(Author, %{name: "Alice", age: 30, rating: 4.5, active: true})
      article = Ash.Seed.seed!(Article, %{title: "Test", word_count: 100, author_id: author.id})
      loaded = Ash.load!(article, :author_name)
      assert loaded.author_name == "Alice"
    end
  end
end
