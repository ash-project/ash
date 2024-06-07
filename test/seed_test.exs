defmodule Ash.Test.SeedTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Seed
  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule Tenant do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true, allow_nil?: false
    end
  end

  defmodule ContextUser do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy :context
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true, allow_nil?: false
    end
  end

  defmodule AttributeUser do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy :attribute
      attribute :tenant_id
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true, allow_nil?: false
    end

    relationships do
      belongs_to :tenant, Ash.Test.SeedTest.Tenant
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, default: "Fred", public?: true
    end

    relationships do
      has_many :posts, Ash.Test.SeedTest.Post, destination_attribute: :author_id, public?: true

      has_one :latest_post, Ash.Test.SeedTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc],
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :category, :string do
        public?(true)
      end

      timestamps()
    end

    relationships do
      belongs_to :author, Author, public?: true

      has_many :ratings, Ash.Test.SeedTest.Rating do
        public?(true)
        domain(Ash.Test.SeedTest.Domain2)
      end

      many_to_many :categories, Ash.Test.SeedTest.Category,
        public?: true,
        through: Ash.Test.SeedTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false, public?: true

      belongs_to :category, Ash.Test.SeedTest.Category,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :posts, Post,
        public?: true,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
  end

  defmodule Rating do
    use Ash.Resource,
      domain: Ash.Test.SeedTest.Domain2,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :rating, :integer do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        domain(Domain)
      end
    end
  end

  defmodule Domain2 do
    @moduledoc false
    use Ash.Domain

    resources do
      resource(Rating)
    end
  end

  describe "seed!/1" do
    test "it creates a single record with resource and input" do
      assert %Post{id: id, title: "seeded"} = seed!(Post, %{title: "seeded"})

      assert post = Ash.get!(Post, id)
      assert post.title == "seeded"
    end

    test "it creates a single record with a struct" do
      assert %Post{id: id, title: "seeded"} = seed!(%Post{title: "seeded"})

      assert post = Ash.get!(Post, id)
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
             } = Post |> Ash.get!(id) |> Ash.load!([:categories, :author, :ratings])

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
             } = Post |> Ash.get!(id) |> Ash.load!([:categories, :author, :ratings])

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
             } = Post |> Ash.get!(id) |> Ash.load!([:categories, :author, :ratings])

      assert categories |> Enum.map(& &1.name) |> Enum.sort() == ["bar", "foo"]
      assert ratings |> Enum.map(& &1.rating) |> Enum.sort() == [1, 2]
      assert author.name == "ted dansen"

      assert Enum.count(Ash.read!(Category)) == 2
      assert Enum.count(Ash.read!(Rating)) == 2
      assert Enum.count(Ash.read!(Author)) == 1
    end

    test "context multitenancy works" do
      tenant = seed!(%Tenant{name: "Tenant"})
      tenant_id = tenant.id

      user = seed!(ContextUser, %{name: "User"}, tenant: tenant_id)
      user_id = user.id

      assert {:ok, %ContextUser{id: ^user_id}} = Ash.get(ContextUser, user_id, tenant: tenant_id)

      assert {:error, %{errors: [%Ash.Error.Query.NotFound{}]}} =
               Ash.get(ContextUser, user_id, tenant: "random")
    end

    test "attribute multitenancy works" do
      tenant = seed!(%Tenant{name: "Tenant"})
      tenant_id = tenant.id

      user = seed!(AttributeUser, %{name: "User"}, tenant: tenant_id)
      user_id = user.id

      assert {:ok, %AttributeUser{id: ^user_id}} =
               Ash.get(AttributeUser, user_id, tenant: tenant_id)

      assert {:error, %{errors: [%Ash.Error.Query.NotFound{}]}} =
               Ash.get(AttributeUser, user_id, tenant: "random")
    end
  end
end
