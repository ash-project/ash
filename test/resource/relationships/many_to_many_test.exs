defmodule Ash.Test.Resource.Relationships.ManyToManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasMany
  alias Ash.Resource.Relationships.ManyToMany
  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "it creates a relationship and a join relationship" do
      defposts do
        relationships do
          many_to_many :related_posts, __MODULE__,
            through: SomeResource,
            source_attribute_on_join_resource: :post_id,
            destination_attribute_on_join_resource: :related_post_id,
            public?: true

          many_to_many :unrelated_posts, __MODULE__,
            through: Tabloid,
            source_attribute_on_join_resource: :post_id,
            destination_attribute_on_join_resource: :unrelated_post_id
        end
      end

      assert [
               %HasMany{
                 cardinality: :many,
                 destination: Tabloid,
                 destination_attribute: :post_id,
                 name: :unrelated_posts_join_assoc,
                 source: Post,
                 source_attribute: :id,
                 type: :has_many,
                 public?: false
               },
               %HasMany{
                 cardinality: :many,
                 destination: SomeResource,
                 destination_attribute: :post_id,
                 name: :related_posts_join_assoc,
                 source: Post,
                 source_attribute: :id,
                 type: :has_many,
                 public?: false
               },
               %ManyToMany{
                 cardinality: :many,
                 destination: Post,
                 destination_attribute: :id,
                 destination_attribute_on_join_resource: :related_post_id,
                 name: :related_posts,
                 source: Post,
                 source_attribute: :id,
                 source_attribute_on_join_resource: :post_id,
                 through: SomeResource,
                 type: :many_to_many,
                 public?: true
               },
               %ManyToMany{
                 cardinality: :many,
                 destination: Post,
                 destination_attribute: :id,
                 destination_attribute_on_join_resource: :unrelated_post_id,
                 name: :unrelated_posts,
                 source: Post,
                 source_attribute: :id,
                 source_attribute_on_join_resource: :post_id,
                 through: Tabloid,
                 type: :many_to_many,
                 public?: false
               }
             ] = Ash.Resource.Info.relationships(Post)

      assert [%ManyToMany{name: :related_posts}] = Ash.Resource.Info.public_relationships(Post)

      assert %ManyToMany{name: :related_posts} =
               Ash.Resource.Info.public_relationship(Post, :related_posts)

      assert nil == Ash.Resource.Info.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.Info.public_relationship(Post, :unrelated_posts)
    end
  end

  describe "validation" do
    test "it fails if you pass a string to `through`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected module in :through option, got: "some_table"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: "some_table",
                source_attribute_on_join_resource: :source_post_id,
                destination_attribute_on_join_resource: :destination_post_id,
                public?: true
            end
          end
        end
      )
    end

    test "you can pass a module to `through`" do
      defposts do
        relationships do
          many_to_many :foobars, Foobar,
            through: FooBars,
            source_attribute_on_join_resource: :source_post_id,
            destination_attribute_on_join_resource: :destination_post_id,
            public?: true
        end
      end
    end

    test "it guesses `source_attribute_on_join_resource` if not set" do
      defposts do
        relationships do
          many_to_many :authors, Author do
            through PostsJoinArticlesAuthors
            destination_attribute_on_join_resource :manager_id
            public?(true)
          end
        end
      end

      assert [
               %HasMany{},
               %ManyToMany{
                 destination: Author,
                 destination_attribute_on_join_resource: :manager_id,
                 source_attribute_on_join_resource: :post_id,
                 through: PostsJoinArticlesAuthors
               }
             ] = Ash.Resource.Info.relationships(Post)
    end

    test "it guesses `destination_attribute_on_join_resource` if not set" do
      defposts do
        relationships do
          many_to_many :authors, ArticleAuthor do
            through PostsJoinArticlesAuthors
            source_attribute_on_join_resource :article_id
            public?(true)
          end
        end
      end

      assert [
               %HasMany{},
               %ManyToMany{
                 destination: ArticleAuthor,
                 destination_attribute_on_join_resource: :article_author_id,
                 source_attribute_on_join_resource: :article_id,
                 through: PostsJoinArticlesAuthors
               }
             ] = Ash.Resource.Info.relationships(Post)
    end

    test "it fails if you dont pass an atom for `source_attribute_on_join_resource`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :source_attribute_on_join_resource option: expected atom, got: "what"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                source_attribute_on_join_resource: "what",
                destination_attribute_on_join_resource: :destination_post_id,
                public?: true
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `destination_attribute_on_join_resource`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :destination_attribute_on_join_resource option: expected atom, got: "what"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBar,
                destination_attribute_on_join_resource: "what",
                source_attribute_on_join_resource: :source_post_id,
                public?: true
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `source_attribute`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :source_attribute option: expected atom, got: "what"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBar,
                source_attribute: "what",
                source_attribute_on_join_resource: :source_post_id,
                destination_attribute_on_join_resource: :destination_post_id,
                public?: true
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `destination_attribute`" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :destination_attribute option: expected atom, got: "what"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                destination_attribute: "what",
                source_attribute_on_join_resource: :source_post_id,
                destination_attribute_on_join_resource: :destination_post_id,
                public?: true
            end
          end
        end
      )
    end

    test "fails if public? is not an boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :public\? option: expected boolean, got: "an_invalid_field"/,
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                source_attribute_on_join_resource: :source_post_id,
                destination_attribute_on_join_resource: :destination_post_id,
                public?: "an_invalid_field"
            end
          end
        end
      )
    end
  end
end
