defmodule Ash.Test.Resource.Relationships.HasOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasOne

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "it creates a relationship" do
      defposts do
        relationships do
          has_one :foo, Foo, destination_attribute: :post_id, public?: true
          has_one :bar, Bar, destination_attribute: :post_id
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
                 public?: true
               },
               %HasOne{
                 cardinality: :one,
                 destination: Bar,
                 destination_attribute: :post_id,
                 name: :bar,
                 source_attribute: :id,
                 type: :has_one,
                 public?: false
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
        ~r/invalid value for :destination_attribute option: expected atom, got: "foo"/,
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, destination_attribute: "foo", public?: true
            end
          end
        end
      )
    end

    test "fails if source_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :source_attribute option: expected atom, got: "foo"/,
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar,
                source_attribute: "foo",
                destination_attribute: :post_id,
                public?: true
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected module in :destination option, got: "foobar"/,
        fn ->
          defposts do
            relationships do
              has_one :foobar, "foobar" do
                public?(true)
              end
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :name option: expected atom, got: "foobar"/,
        fn ->
          defposts do
            relationships do
              has_one "foobar", Foobar do
                public?(true)
              end
            end
          end
        end
      )
    end

    test "fails if public? is not an boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :public\? option: expected boolean, got: "foo"/,
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar,
                public?: "foo",
                destination_attribute: :post_id
            end
          end
        end
      )
    end
  end
end
