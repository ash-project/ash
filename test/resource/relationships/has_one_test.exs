defmodule Ash.Test.Resource.Relationships.HasOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasOne

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

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
          has_one :foo, Foo, destination_attribute: :post_id
          has_one :bar, Bar, destination_attribute: :post_id, private?: true
        end
      end

      assert [
               %HasOne{
                 cardinality: :one,
                 destination: Foo,
                 destination_attribute: :post_id,
                 name: :foo,
                 source_attribute: :id,
                 type: :has_one,
                 private?: false
               },
               %HasOne{
                 cardinality: :one,
                 destination: Bar,
                 destination_attribute: :post_id,
                 name: :bar,
                 source_attribute: :id,
                 type: :has_one,
                 private?: true
               }
             ] = Ash.Resource.Info.relationships(Post)

      assert [%HasOne{name: :foo}] = Ash.Resource.Info.public_relationships(Post)

      assert %HasOne{name: :foo} = Ash.Resource.Info.public_relationship(Post, :foo)

      assert nil == Ash.Resource.Info.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.Info.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasOneTest.Post]\n relationships -> has_one -> foobar:\n  invalid value for :destination_attribute option: expected atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, destination_attribute: "foo"
            end
          end
        end
      )
    end

    test "fails if source_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasOneTest.Post]\n relationships -> has_one -> foobar:\n  invalid value for :source_attribute option: expected atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, source_attribute: "foo", destination_attribute: :post_id
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasOneTest.Post]\n relationships -> has_one -> foobar:\n  expected module in :destination option, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, "foobar"
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasOneTest.Post]\n relationships -> has_one -> foobar:\n  invalid value for :name option: expected atom, got: \"foobar\"",
        fn ->
          defposts do
            relationships do
              has_one "foobar", Foobar
            end
          end
        end
      )
    end

    test "fails if private? is not an boolean" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.HasOneTest.Post]\n relationships -> has_one -> foobar:\n  invalid value for :private? option: expected boolean, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, private?: "foo", destination_attribute: :post_id
            end
          end
        end
      )
    end
  end
end
