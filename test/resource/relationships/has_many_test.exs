defmodule Ash.Test.Resource.Relationshihps.HasManyTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post", primary_key: false

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates a relationship" do
      defposts do
        relationships do
          has_many :foobar, FooBar
        end
      end

      assert [
               %Ash.Resource.Relationships.HasMany{
                 cardinality: :many,
                 destination: FooBar,
                 destination_field: :post_id,
                 name: :foobar,
                 source_field: :id,
                 type: :has_many
               }
             ] = Ash.relationships(Post)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option destination_field at relationships -> has_many -> foobar must be atom",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, destination_field: "foo"
            end
          end
        end
      )
    end

    test "fails if source_field is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option source_field at relationships -> has_many -> foobar must be atom",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, source_field: "foo"
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "related resource must be a module representing a resource at relationships -> has_many -> foobar",
        fn ->
          defposts do
            relationships do
              has_many :foobar, "foobar"
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationship_name must be an atom at relationships -> has_many",
        fn ->
          defposts do
            relationships do
              has_many "foobar", Foobar
            end
          end
        end
      )
    end
  end
end
