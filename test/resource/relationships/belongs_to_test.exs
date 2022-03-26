defmodule Ash.Test.Resource.Relationships.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.BelongsTo

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
    test "it creates an attribute" do
      defposts do
        relationships do
          belongs_to(:foobar, FooBar)
        end
      end

      assert [
               _,
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 private?: true
               }
             ] = Ash.Resource.Info.attributes(Post)
    end

    test "it creates an attribute that honors private?" do
      defposts do
        relationships do
          belongs_to(:foobar, FooBar, private?: true)
        end
      end

      assert [
               _,
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 private?: true
               }
             ] = Ash.Resource.Info.attributes(Post)
    end

    test "it creates a relationship" do
      defposts do
        relationships do
          belongs_to(:foo, Foo)
          belongs_to(:bar, Bar, source_field: :bazz, private?: true)
        end
      end

      assert [
               %BelongsTo{
                 cardinality: :one,
                 define_field?: true,
                 destination: Foo,
                 destination_field: :id,
                 field_type: :uuid,
                 name: :foo,
                 primary_key?: false,
                 source_field: :foo_id,
                 type: :belongs_to,
                 private?: false
               },
               %BelongsTo{
                 cardinality: :one,
                 define_field?: true,
                 destination: Bar,
                 destination_field: :id,
                 field_type: :uuid,
                 name: :bar,
                 primary_key?: false,
                 source_field: :bazz,
                 type: :belongs_to,
                 private?: true
               }
             ] = Ash.Resource.Info.relationships(Post)

      assert [%BelongsTo{name: :foo}] = Ash.Resource.Info.public_relationships(Post)

      assert %BelongsTo{name: :foo} = Ash.Resource.Info.public_relationship(Post, :foo)

      assert nil == Ash.Resource.Info.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.Info.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :destination_field to be an atom, got: \"foo\"",
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
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :source_field to be an atom, got: \"foo\"",
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
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :destination to be an atom, got: \"foobar\"",
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
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :name to be an atom, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              belongs_to("foobar", Foobar)
            end
          end
        end
      )
    end

    test "fails if the relationship and an attribute have the same name" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n Attribute and relationship with name `foobar` are defined in Ash.Test.Resource.Relationships.BelongsToTest.Post\n\nA resource cannot have an attribute and relationship with the same name.\n",
        fn ->
          defposts do
            attributes do
              attribute(:foobar, :string)
            end

            relationships do
              belongs_to(:foobar, Foobar)
            end
          end
        end
      )
    end

    test "fails if `primary_key?` is not a boolean" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :primary_key? to be a boolean, got: \"blah\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, Foobar, primary_key?: "blah")
            end
          end
        end
      )
    end

    test "fails if `private?` is not a boolean" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :private? to be a boolean, got: \"blah\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, Foobar, private?: "blah")
            end
          end
        end
      )
    end
  end

  test "fails if `define_field?` is not a boolean" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  expected :define_field? to be a boolean, got: \"blah\"",
      fn ->
        defposts do
          relationships do
            belongs_to(:foobar, Foobar, define_field?: "blah")
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

        defmodule Registry do
          @moduledoc false
          use Ash.Registry

          entries do
            entry Post
          end
        end

        defmodule Api do
          use Ash.Api

          resources do
            registry Registry
          end
        end
      end
    )
  end
end
