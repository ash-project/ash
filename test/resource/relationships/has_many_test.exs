defmodule Ash.Test.Resource.Relationships.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasMany

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
    test "it creates a relationship" do
      defposts do
        relationships do
          has_many :foo, Foo
          has_many :bar, Bar, destination_attribute: :bazz, private?: true
        end
      end

      assert [
               %HasMany{
                 cardinality: :many,
                 destination: Foo,
                 destination_attribute: :post_id,
                 name: :foo,
                 source_attribute: :id,
                 type: :has_many,
                 private?: false
               },
               %HasMany{
                 cardinality: :many,
                 destination: Bar,
                 destination_attribute: :bazz,
                 name: :bar,
                 source_attribute: :id,
                 type: :has_many,
                 private?: true
               }
             ] = Ash.Resource.Info.relationships(Post)

      assert [%HasMany{name: :foo}] = Ash.Resource.Info.public_relationships(Post)

      assert %HasMany{name: :foo} = Ash.Resource.Info.public_relationship(Post, :foo)

      assert nil == Ash.Resource.Info.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.Info.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasManyTest.Post]\n relationships -> has_many -> foobar:\n  expected :destination_attribute to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, destination_attribute: "foo"
            end
          end
        end
      )
    end

    test "fails if source_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasManyTest.Post]\n relationships -> has_many -> foobar:\n  expected :source_attribute to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, source_attribute: "foo", destination_attribute: :post_id
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasManyTest.Post]\n relationships -> has_many -> foobar:\n  expected :destination to be an atom, got: \"foobar\"",
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
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasManyTest.Post]\n relationships -> has_many -> foobar:\n  expected :name to be an atom, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              has_many "foobar", Foobar
            end
          end
        end
      )
    end

    test "fails if private? is not an boolean" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasManyTest.Post]\n relationships -> has_many -> foobar:\n  expected :private? to be a boolean, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, private?: "foo", destination_attribute: :post_id
            end
          end
        end
      )
    end
  end
end
