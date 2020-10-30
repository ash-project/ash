defmodule Ash.Test.Resource.Relationshihps.HasManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.HasMany

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
          has_many :foo, Foo, destination_field: :post_id
          has_many :bar, Bar, destination_field: :bazz, private?: true
        end
      end

      assert [
               %HasMany{
                 cardinality: :many,
                 destination: Foo,
                 destination_field: :post_id,
                 name: :foo,
                 source_field: :id,
                 type: :has_many,
                 private?: false
               },
               %HasMany{
                 cardinality: :many,
                 destination: Bar,
                 destination_field: :bazz,
                 name: :bar,
                 source_field: :id,
                 type: :has_many,
                 private?: true
               }
             ] = Ash.Resource.relationships(Post)

      assert [%HasMany{name: :foo}] = Ash.Resource.public_relationships(Post)

      assert %HasMany{name: :foo} = Ash.Resource.public_relationship(Post, :foo)

      assert nil == Ash.Resource.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_field is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasMany]\n relationships -> has_many -> foobar:\n  expected :destination_field to be an atom, got: \"foo\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasMany]\n relationships -> has_many -> foobar:\n  expected :source_field to be an atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, source_field: "foo", destination_field: :post_id
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasMany]\n relationships -> has_many -> foobar:\n  expected :destination to be an atom, got: \"foobar\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasMany]\n relationships -> has_many -> foobar:\n  expected :name to be an atom, got: \"foobar\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Dsl.HasMany]\n relationships -> has_many -> foobar:\n  expected :private? to be an boolean, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              has_many :foobar, FooBar, private?: "foo", destination_field: :post_id
            end
          end
        end
      )
    end
  end
end
