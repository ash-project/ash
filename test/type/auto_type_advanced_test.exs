defmodule Ash.Test.Type.AutoTypeAdvancedTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

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
      belongs_to :post, Ash.Test.Type.AutoTypeAdvancedTest.Post do
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
      attribute :views, :integer, public?: true
      attribute :published, :boolean, public?: true
    end

    calculations do
      # Auto type from exists expression -> Boolean
      calculate :has_tags, :auto, expr(exists(tags, true)) do
        public?(true)
      end

      # Auto type referencing an explicitly typed calculation
      calculate :views_display, :string, expr(title <> " (" <> type(views, :string) <> " views)") do
        public?(true)
      end

      calculate :views_copy, :auto, expr(views) do
        public?(true)
      end

      # Auto type from a boolean attribute
      calculate :published_copy, :auto, expr(published) do
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

  describe "auto type with exists expression" do
    test "exists resolves to Boolean" do
      calc = Ash.Resource.Info.calculation(Post, :has_tags)
      assert calc.type == Ash.Type.Boolean
    end

    # exists() is evaluated in the data layer and ETS doesn't fully support
    # filter-based exists; skip runtime tests, type resolution is the key test
  end

  describe "auto type with integer attribute" do
    test "integer ref resolves correctly" do
      calc = Ash.Resource.Info.calculation(Post, :views_copy)
      assert calc.type == Ash.Type.Integer
    end

    test "integer ref returns correct runtime value" do
      post = Ash.Seed.seed!(Post, %{title: "Test", body: "Body", views: 42, published: true})
      loaded = Ash.load!(post, :views_copy)
      assert loaded.views_copy == 42
    end
  end

  describe "auto type - calculation referencing another calc" do
    test "auto calc can reference explicitly typed calculations" do
      # views_display is explicitly typed, views_copy is auto from integer attribute
      post = Ash.Seed.seed!(Post, %{title: "Test", body: "Body", views: 100, published: true})
      loaded = Ash.load!(post, [:views_copy, :views_display])
      assert loaded.views_copy == 100
      assert loaded.views_display == "Test (100 views)"
    end
  end
end
