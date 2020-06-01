defmodule Ash.Test.Resource.Relationships.BelongsToTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post"

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates an attribute" do
      defposts do
        relationships do
          belongs_to :foobar, FooBar
        end
      end

      assert [
               %Ash.Resource.Attributes.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: :uuid
               }
             ] = Ash.attributes(Post)
    end

    test "it creates a relationship" do
      defposts do
        relationships do
          belongs_to :foobar, FooBar
        end
      end

      assert [
               %Ash.Resource.Relationships.BelongsTo{
                 cardinality: :one,
                 define_field?: true,
                 destination: FooBar,
                 destination_field: :id,
                 field_type: :uuid,
                 name: :foobar,
                 primary_key?: false,
                 source_field: :foobar_id,
                 type: :belongs_to
               }
             ] = Ash.relationships(Post)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> belongs_to -> foobar:\n  expected :destination_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to :foobar, FooBar, destination_field: "foo"
            end
          end
        end
      )
    end

    test "fails if source_field is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> belongs_to -> foobar:\n  expected :source_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to :foobar, FooBar, source_field: "foo"
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> belongs_to -> foobar:\n  related resource must be a module representing a resource",
        fn ->
          defposts do
            relationships do
              belongs_to :foobar, "foobar"
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> belongs_to:\n  relationship_name must be an atom",
        fn ->
          defposts do
            relationships do
              belongs_to "foobar", Foobar
            end
          end
        end
      )
    end

    test "fails if `primary_key?` is not a boolean" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "relationships -> belongs_to -> foobar:\n  expected :primary_key? to be an boolean, got: \"blah\"",
        fn ->
          defposts do
            relationships do
              belongs_to :foobar, Foobar, primary_key?: "blah"
            end
          end
        end
      )
    end
  end

  test "fails if `define_field?` is not a boolean" do
    assert_raise(
      Ash.Error.ResourceDslError,
      "relationships -> belongs_to -> foobar:\n  expected :define_field? to be an boolean, got: \"blah\"",
      fn ->
        defposts do
          relationships do
            belongs_to :foobar, Foobar, define_field?: "blah"
          end
        end
      end
    )
  end

  test "fails if `field_type` is not an atom" do
    assert_raise(
      Ash.Error.ResourceDslError,
      "relationships -> belongs_to -> foobar:\n  \"foo\" is not a valid type",
      fn ->
        defposts do
          relationships do
            belongs_to :foobar, Foobar, field_type: "foo"
          end
        end
      end
    )
  end
end
