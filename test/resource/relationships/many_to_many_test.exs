defmodule Ash.Test.Resource.Relationships.ManyToManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
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
        end
      end

      assert [
               %Ash.Resource.Relationships.HasMany{
                 cardinality: :many,
                 destination: SomeResource,
                 destination_field: :post_id,
                 name: :related_posts_join_assoc,
                 source: Ash.Test.Resource.Relationships.ManyToManyTest.Post,
                 source_field: :id,
                 type: :has_many
               },
               %Ash.Resource.Relationships.ManyToMany{
                 cardinality: :many,
                 destination: Ash.Test.Resource.Relationships.ManyToManyTest.Post,
                 destination_field: :id,
                 destination_field_on_join_table: :related_post_id,
                 name: :related_posts,
                 source: Ash.Test.Resource.Relationships.ManyToManyTest.Post,
                 source_field: :id,
                 source_field_on_join_table: :post_id,
                 through: SomeResource,
                 type: :many_to_many
               }
             ] = Ash.Resource.relationships(Post)
    end
  end

  describe "validation" do
    test "it fails if you pass a string to `through`" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> many_to_many -> foobars:\n  expected :through to be an atom, got: \"some_table\"",
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
        Ash.Error.ResourceDslError,
        "relationships -> many_to_many -> foobars:\n  expected :source_field_on_join_table to be an atom, got: \"what\"",
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
        Ash.Error.ResourceDslError,
        "relationships -> many_to_many -> foobars:\n  expected :destination_field_on_join_table to be an atom, got: \"what\"",
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
        Ash.Error.ResourceDslError,
        "relationships -> many_to_many -> foobars:\n  expected :source_field to be an atom, got: \"what\"",
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
        Ash.Error.ResourceDslError,
        "relationships -> many_to_many -> foobars:\n  expected :destination_field to be an atom, got: \"what\"",
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
  end
end
