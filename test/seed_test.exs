defmodule Ash.Test.SeedTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Seed
  require Ash.Query
  alias Ash.Test.AnyApi, as: Api

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, default: "Fred"
    end

    relationships do
      has_many :posts, Ash.Test.SeedTest.Post, destination_attribute: :author_id

      has_one :latest_post, Ash.Test.SeedTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :category, :string
      timestamps()
    end

    relationships do
      belongs_to :author, Author

      has_many :ratings, Ash.Test.SeedTest.Rating do
        api Ash.Test.SeedTest.Api2
      end

      many_to_many :categories, Ash.Test.SeedTest.Category,
        through: Ash.Test.SeedTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false

      belongs_to :category, Ash.Test.SeedTest.Category,
        primary_key?: true,
        allow_nil?: false
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      many_to_many :posts, Post,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
  end

  defmodule Rating do
    use Ash.Resource,
      api: Ash.Test.SeedTest.Api2,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :rating, :integer
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post do
        api Api
      end
    end
  end

  defmodule Api2 do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Rating)
    end
  end

  describe "seed!/1" do
    test "it creates a single record with resource and input" do
      assert %Post{id: id, title: "seeded"} = seed!(Post, %{title: "seeded"})

      assert post = Api.get!(Post, id)
      assert post.title == "seeded"
    end

    test "it creates a single record with a struct" do
      assert %Post{id: id, title: "seeded"} = seed!(%Post{title: "seeded"})

      assert post = Api.get!(Post, id)
      assert post.title == "seeded"
    end

    test "defaults are set when using a struct" do
      assert %Author{name: "Fred"} = seed!(%Author{})
      assert %Author{name: "Fred"} = seed!(%Author{})
    end

    test "it creates related entities" do
      assert %Post{
               id: id,
               title: "seeded",
               categories: [%Category{name: "foo"}, %Category{name: "bar"}],
               author: %Author{name: "ted dansen"},
               ratings: [%Rating{rating: 1}, %Rating{rating: 2}]
             } =
               seed!(%Post{
                 title: "seeded",
                 categories: [%Category{name: "foo"}, %Category{name: "bar"}],
                 author: %Author{name: "ted dansen"},
                 ratings: [%Rating{rating: 1}, %Rating{rating: 2}]
               })

      assert %Post{
               id: ^id,
               title: "seeded",
               categories: categories,
               author: %Author{name: "ted dansen"},
               ratings: ratings
             } = Post |> Api.get!(id) |> Api.load!([:categories, :author, :ratings])

      assert categories |> Enum.map(& &1.name) |> Enum.sort() == ["bar", "foo"]
      assert ratings |> Enum.map(& &1.rating) |> Enum.sort() == [1, 2]
    end

    test "it reuses entities that have been loaded (doesnt try to create a copy)" do
      assert %Post{
               id: id,
               title: "seeded",
               categories: [%Category{name: "foo"}, %Category{name: "bar"}],
               author: %Author{name: "ted dansen"},
               ratings: [%Rating{rating: 1}, %Rating{rating: 2}]
             } =
               seed!(%Post{
                 title: "seeded",
                 categories: [%Category{name: "foo"}, %Category{name: "bar"}],
                 author: %Author{name: "ted dansen"},
                 ratings: [%Rating{rating: 1}, %Rating{rating: 2}]
               })

      assert %Post{
               id: ^id,
               title: "seeded",
               categories: categories,
               author: author,
               ratings: ratings
             } = Post |> Api.get!(id) |> Api.load!([:categories, :author, :ratings])

      assert %Post{id: id} =
               seed!(%Post{
                 title: "seeded2",
                 categories: categories,
                 author: author,
                 ratings: ratings
               })

      assert %Post{
               id: ^id,
               title: "seeded2",
               categories: categories,
               author: author,
               ratings: ratings
             } = Post |> Api.get!(id) |> Api.load!([:categories, :author, :ratings])

      assert categories |> Enum.map(& &1.name) |> Enum.sort() == ["bar", "foo"]
      assert ratings |> Enum.map(& &1.rating) |> Enum.sort() == [1, 2]
      assert author.name == "ted dansen"

      assert Enum.count(Api.read!(Category)) == 2
      assert Enum.count(Api.read!(Rating)) == 2
      assert Enum.count(Api.read!(Author)) == 1
    end
  end
end
