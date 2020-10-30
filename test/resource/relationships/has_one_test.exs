defmodule Ash.Test.Resource.Relationshihps.HasOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasOne

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
    test "it creates a relationship" do
      defposts do
        relationships do
          has_one :foo, Foo, destination_field: :post_id
          has_one :bar, Bar, destination_field: :post_id, private?: true
        end
      end

      assert [
               %HasOne{
                 cardinality: :one,
                 destination: Foo,
                 destination_field: :post_id,
                 name: :foo,
                 source_field: :id,
                 type: :has_one,
                 private?: false
               },
               %HasOne{
                 cardinality: :one,
                 destination: Bar,
                 destination_field: :post_id,
                 name: :bar,
                 source_field: :id,
                 type: :has_one,
                 private?: true
               }
             ] = Ash.Resource.relationships(Post)

      assert [%HasOne{name: :foo}] = Ash.Resource.public_relationships(Post)

      assert %HasOne{name: :foo} = Ash.Resource.public_relationship(Post, :foo)

      assert nil == Ash.Resource.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasOne]\n relationships -> has_one -> foobar:\n  expected :destination_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, destination_field: "foo"
            end
          end
        end
      )
    end

    test "fails if source_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasOne]\n relationships -> has_one -> foobar:\n  expected :source_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, source_field: "foo", destination_field: :post_id
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasOne]\n relationships -> has_one -> foobar:\n  expected :destination to be an atom, got: \"foobar\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasOne]\n relationships -> has_one -> foobar:\n  expected :name to be an atom, got: \"foobar\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasOne]\n relationships -> has_one -> foobar:\n  expected :private? to be an boolean, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_one :foobar, FooBar, private?: "foo", destination_field: :post_id
            end
          end
        end
      )
    end
  end
end
