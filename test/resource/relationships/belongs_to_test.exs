defmodule Ash.Test.Resource.Relationships.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          attribute(:id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0)
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates an attribute" do
      defposts do
        relationships do
          belongs_to(:foobar, FooBar)
        end
      end

      assert [
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID
               },
               _
             ] = Ash.Resource.attributes(Post)
    end

    test "it creates a relationship" do
      defposts do
        relationships do
          belongs_to(:foobar, FooBar)
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
             ] = Ash.Resource.relationships(Post)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "relationships -> belongs_to -> foobar:\n  expected :destination_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, destination_field: "foo")
            end
          end
        end
      )
    end

    test "fails if source_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "relationships -> belongs_to -> foobar:\n  expected :source_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, source_field: "foo")
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "relationships -> belongs_to -> foobar:\n  expected :destination to be an atom, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, "foobar")
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "relationships -> belongs_to -> foobar:\n  expected :name to be an atom, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              belongs_to("foobar", Foobar)
            end
          end
        end
      )
    end

    test "fails if `primary_key?` is not a boolean" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "relationships -> belongs_to -> foobar:\n  expected :primary_key? to be an boolean, got: \"blah\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, Foobar, primary_key?: "blah")
            end
          end
        end
      )
    end
  end

  test "fails if `define_field?` is not a boolean" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      "relationships -> belongs_to -> foobar:\n  expected :define_field? to be an boolean, got: \"blah\"",
      fn ->
        defposts do
          relationships do
            belongs_to(:foobar, Foobar, define_field?: "blah")
          end
        end
      end
    )
  end

  test "fails if `field_type` is not an atom" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      "relationships -> belongs_to -> foobar:\n  Attribute type must be a built in type or a type module, got: \"foo\"",
      fn ->
        defposts do
          relationships do
            belongs_to(:foobar, Foobar, field_type: "foo")
          end
        end
      end
    )
  end

  test "fails in api initialization if the destination resource doesn't have the correct field" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      ~r/Relationship `post` expects source field `post_id` to be defined/,
      fn ->
        defposts do
          relationships do
            belongs_to(:post, __MODULE__, define_field?: false)
          end
        end

        defmodule Api do
          use Ash.Api

          resources do
            resource(Post)
          end
        end
      end
    )
  end
end
