defmodule Ash.Test.GeneratorTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Ash.Seed
  require Ash.Query
  alias Ash.Test.Domain, as: Domain

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
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, default: "Fred", public?: true

      attribute :metadata, :map do
        public?(true)
        allow_nil? true
      end

      attribute :meta, :map do
        public?(true)
        allow_nil? false
      end
    end

    relationships do
      has_many :posts, Ash.Test.GeneratorTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_one :latest_post, Ash.Test.GeneratorTest.Post,
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
      defaults [:create, :read, :update, :destroy]
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

      has_many :ratings, Ash.Test.GeneratorTest.Rating, public?: true

      many_to_many :categories, Ash.Test.GeneratorTest.Category,
        through: Ash.Test.GeneratorTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id,
        public?: true
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
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false, public?: true

      belongs_to :category, Ash.Test.GeneratorTest.Category,
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

    identities do
      identity :unique_name, [:name], pre_check_with: Domain
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
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
      domain: Domain,
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
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
        domain(Ash.Test.GeneratorTest.Category)
      end
    end
  end

  describe "action_input" do
    test "action input can be provided to an action" do
      check all(input <- Ash.Generator.action_input(Post, :create)) do
        Post
        |> Ash.Changeset.for_create(:create, input)
        |> Ash.create!()
      end
    end

    test "overrides are applied" do
      check all(
              input <-
                Ash.Generator.action_input(Post, :create, %{title: "text"})
            ) do
        post =
          Post
          |> Ash.Changeset.for_create(:create, input)
          |> Ash.create!()

        assert post.title == "text"
      end
    end
  end

  test "string generator honors trim?: true" do
    check all(string <- Ash.Type.String.generator(min_length: 5, trim?: true)) do
      assert String.length(String.trim(string)) >= 5
    end
  end

  describe "changeset" do
    test "a directly usable changeset can be created" do
      Post
      |> Ash.Generator.changeset(:create)
      |> Ash.create!()
    end

    test "many changesets can be generated" do
      posts =
        Post
        |> Ash.Generator.many_changesets(:create, 5)
        |> Enum.map(&Ash.create!/1)

      assert Enum.count(posts) == 5
    end
  end

  @meta_generator %{
    meta: %{},
    metadata: %{}
  }
  describe "seed_input" do
    test "it returns attributes generated" do
      Author
      |> Ash.Generator.seed_input(@meta_generator)
      |> Enum.take(10)
      |> Enum.each(fn input ->
        seed!(Author, input)
      end)
    end

    test "it can be used in property testing" do
      check all(input <- Ash.Generator.seed_input(Author, @meta_generator)) do
        seed!(Author, input)
      end
    end

    test "many seeds can be generated with seed_many!" do
      assert Enum.count(Ash.Generator.seed_many!(Author, 5, @meta_generator)) == 5
    end
  end

  describe "built in generators" do
    for type <- Enum.uniq(Ash.Type.builtin_types()), type != Ash.Type.Keyword do
      for type <- [{:array, type}, type] do
        constraints =
          case type do
            {:array, type} ->
              [items: Spark.Options.validate!([], Ash.Type.constraints(type))]

            type ->
              Spark.Options.validate!([], Ash.Type.constraints(type))
          end

        test "#{inspect(type)} type can be generated" do
          try do
            check all(input <- Ash.Type.generator(unquote(type), unquote(constraints))) do
              {:ok, _} = Ash.Type.cast_input(unquote(type), input, unquote(constraints))
            end
          rescue
            e in RuntimeError ->
              case e do
                %RuntimeError{message: "generator/1 unimplemented for" <> _} ->
                  :ok

                other ->
                  reraise other, __STACKTRACE__
              end
          end
        end
      end
    end
  end
end
