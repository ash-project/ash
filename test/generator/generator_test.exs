defmodule Ash.Test.GeneratorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Seed
  require Ash.Query
  use ExUnitProperties

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
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
      has_many :posts, Ash.Test.GeneratorTest.Post, destination_attribute: :author_id

      has_one :latest_post, Ash.Test.GeneratorTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

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

      has_many :ratings, Ash.Test.GeneratorTest.Rating

      many_to_many :categories, Ash.Test.GeneratorTest.Category,
        through: Ash.Test.GeneratorTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, required?: true

      belongs_to :category, Ash.Test.GeneratorTest.Category,
        primary_key?: true,
        required?: true
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Ash.Test.GeneratorTest.Api
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
        api Ash.Test.GeneratorTest.Category
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
      entry(Post)
      entry(Category)
      entry(Rating)
      entry(PostCategory)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "action_input" do
    test "action input can be provided to an action" do
      check all(input <- Ash.Generator.action_input(Post, :create)) do
        Post
        |> Ash.Changeset.for_create(:create, input)
        |> Api.create!()
      end
    end

    test "overrides are applied" do
      check all(input <- Ash.Generator.action_input(Post, :create, %{title: "text"})) do
        post =
          Post
          |> Ash.Changeset.for_create(:create, input)
          |> Api.create!()

        assert post.title == "text"
      end
    end
  end

  describe "changeset" do
    test "a directly useable changeset can be created" do
      Post
      |> Ash.Generator.changeset(:create)
      |> Api.create!()
    end

    test "many changesets can be generated" do
      posts =
        Post
        |> Ash.Generator.many_changesets(:create, 5)
        |> Enum.map(&Api.create!/1)

      assert Enum.count(posts) == 5
    end
  end

  describe "seed_input" do
    test "it returns attributes generated" do
      Author
      |> Ash.Generator.seed_input()
      |> Enum.take(10)
      |> Enum.each(fn input ->
        seed!(Author, input)
      end)
    end

    test "it can be used in property testing" do
      check all(input <- Ash.Generator.seed_input(Author)) do
        seed!(Author, input)
      end
    end

    test "many seeds can be generated with seed_many!" do
      assert Enum.count(Ash.Generator.seed_many!(Author, 5)) == 5
    end
  end

  describe "built in generators" do
    for type <- Enum.uniq(Enum.map(Ash.Type.builtin_types(), &elem(&1, 1))) do
      Code.ensure_compiled!(type)
      has_generator? = function_exported?(type, :generator, 1)

      for type <- [{:array, type}, type] do
        constraints = Spark.OptionsHelpers.validate!([], Ash.Type.constraints(type))

        if has_generator? do
          test "#{inspect(type)} type can be generated" do
            check all(input <- Ash.Type.generator(unquote(type), unquote(constraints))) do
              {:ok, _} = Ash.Type.cast_input(unquote(type), input, unquote(constraints))
            end
          end
        else
          test "#{inspect(type)} cannot be generated" do
            assert_raise RuntimeError, ~r/generator\/1 unimplemented for/, fn ->
              Ash.Type.generator(unquote(type), unquote(constraints))
            end
          end
        end
      end
    end
  end
end
