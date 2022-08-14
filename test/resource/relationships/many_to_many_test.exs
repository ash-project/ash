defmodule Ash.Test.Resource.Relationships.ManyToManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias __MODULE__
  alias Ash.Resource.Relationships.HasMany
  alias Ash.Resource.Relationships.ManyToMany

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates a relationship and a join relationship" do
      defposts do
        relationships do
          many_to_many :related_posts, Post,
            through: SomeResource,
            source_field_on_join_table: :post_id,
            destination_field_on_join_table: :related_post_id

          many_to_many :unrelated_posts, Post,
            through: Tabloid,
            source_field_on_join_table: :post_id,
            destination_field_on_join_table: :unrelated_post_id,
            private?: true
        end
      end

      assert [
               %HasMany{
                 cardinality: :many,
                 destination: Tabloid,
                 destination_field: :post_id,
                 name: :unrelated_posts_join_assoc,
                 source: ManyToManyTest.Post,
                 source_field: :id,
                 type: :has_many,
                 private?: true
               },
               %HasMany{
                 cardinality: :many,
                 destination: SomeResource,
                 destination_field: :post_id,
                 name: :related_posts_join_assoc,
                 source: ManyToManyTest.Post,
                 source_field: :id,
                 type: :has_many,
                 private?: true
               },
               %ManyToMany{
                 cardinality: :many,
                 destination: ManyToManyTest.Post,
                 destination_field: :id,
                 destination_field_on_join_table: :related_post_id,
                 name: :related_posts,
                 source: ManyToManyTest.Post,
                 source_field: :id,
                 source_field_on_join_table: :post_id,
                 through: SomeResource,
                 type: :many_to_many,
                 private?: false
               },
               %ManyToMany{
                 cardinality: :many,
                 destination: ManyToManyTest.Post,
                 destination_field: :id,
                 destination_field_on_join_table: :unrelated_post_id,
                 name: :unrelated_posts,
                 source: ManyToManyTest.Post,
                 source_field: :id,
                 source_field_on_join_table: :post_id,
                 through: Tabloid,
                 type: :many_to_many,
                 private?: true
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
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :through to be an atom, got: \"some_table\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: "some_table",
                source_field_on_join_table: :source_post_id,
                destination_field_on_join_table: :destination_post_id
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
            source_field_on_join_table: :source_post_id,
            destination_field_on_join_table: :destination_post_id
        end
      end
    end

    test "it fails if you dont pass an atom for `source_field_on_join_table`" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :source_field_on_join_table to be an atom, got: \"what\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                source_field_on_join_table: "what",
                destination_field_on_join_table: :destination_post_id
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `destination_field_on_join_table`" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :destination_field_on_join_table to be an atom, got: \"what\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBar,
                destination_field_on_join_table: "what",
                source_field_on_join_table: :source_post_id
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `source_field`" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :source_field to be an atom, got: \"what\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBar,
                source_field: "what",
                source_field_on_join_table: :source_post_id,
                destination_field_on_join_table: :destination_post_id
            end
          end
        end
      )
    end

    test "it fails if you dont pass an atom for `destination_field`" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :destination_field to be an atom, got: \"what\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                destination_field: "what",
                source_field_on_join_table: :source_post_id,
                destination_field_on_join_table: :destination_post_id
            end
          end
        end
      )
    end

    test "fails if private? is not an boolean" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.ManyToManyTest.Post]\n relationships -> many_to_many -> foobars:\n  expected :private? to be a boolean, got: \"an_invalid_field\"",
        fn ->
          defposts do
            relationships do
              many_to_many :foobars, Foobar,
                through: FooBars,
                source_field_on_join_table: :source_post_id,
                destination_field_on_join_table: :destination_post_id,
                private?: "an_invalid_field"
            end
          end
        end
      )
    end
  end
end
