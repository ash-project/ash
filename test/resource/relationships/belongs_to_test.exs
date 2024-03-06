defmodule Ash.Test.Resource.Relationships.BelongsToTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Relationships.BelongsTo
  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
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
               _,
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 public?: false
               }
             ] = Ash.Resource.Info.attributes(Post)
    end

    test "it creates an attribute that honors attribute_writable?" do
      defposts do
        relationships do
          belongs_to :foobar, FooBar, attribute_writable?: true, public?: true
        end
      end

      assert [
               _,
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 public?: true,
                 writable?: true
               }
             ] = Ash.Resource.Info.attributes(Post)
    end

    test "it creates a relationship" do
      defposts do
        relationships do
          belongs_to(:foo, Foo, public?: true)
          belongs_to(:bar, Bar, source_attribute: :bazz)
        end
      end

      assert [
               %BelongsTo{
                 cardinality: :one,
                 define_attribute?: true,
                 destination: Foo,
                 destination_attribute: :id,
                 attribute_type: :uuid,
                 name: :foo,
                 primary_key?: false,
                 source_attribute: :foo_id,
                 type: :belongs_to,
                 public?: true
               },
               %BelongsTo{
                 cardinality: :one,
                 define_attribute?: true,
                 destination: Bar,
                 destination_attribute: :id,
                 attribute_type: :uuid,
                 name: :bar,
                 primary_key?: false,
                 source_attribute: :bazz,
                 type: :belongs_to,
                 public?: false
               }
             ] = Ash.Resource.Info.relationships(Post)

      assert [%BelongsTo{name: :foo}] = Ash.Resource.Info.public_relationships(Post)

      assert %BelongsTo{name: :foo} = Ash.Resource.Info.public_relationship(Post, :foo)

      assert nil == Ash.Resource.Info.relationship(Post, :definitely_legit_relationship)

      assert nil == Ash.Resource.Info.public_relationship(Post, :bar)
    end
  end

  describe "validations" do
    test "fails if destination_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :destination_attribute option: expected atom, got: \"foo\"/,
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, destination_attribute: "foo", public?: true)
            end
          end
        end
      )
    end

    test "fails if source_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :source_attribute option: expected atom, got: \"foo\"/,
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, source_attribute: "foo", public?: true)
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/expected module in :destination option, got: \"foobar\"/,
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, "foobar", public?: true)
            end
          end
        end
      )
    end

    test "fails if the relationship name is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :name option: expected atom, got: \"foobar\"/,
        fn ->
          defposts do
            relationships do
              belongs_to("foobar", Foobar, public?: true)
            end
          end
        end
      )
    end

    test "fails if `primary_key?` is not a boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :primary_key\? option: expected boolean, got: \"blah\"/,
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, Foobar, primary_key?: "blah", public?: true)
            end
          end
        end
      )
    end

    test "fails if `public?` is not a boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :public\? option: expected boolean, got: "blah"/,
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, Foobar, public?: "blah")
            end
          end
        end
      )
    end
  end

  test "fails if `define_attribute?` is not a boolean" do
    assert_raise(
      Spark.Error.DslError,
      ~r/invalid value for :define_attribute\? option: expected boolean, got: \"blah\"/,
      fn ->
        defposts do
          relationships do
            belongs_to(:foobar, Foobar, define_attribute?: "blah", public?: true)
          end
        end
      end
    )
  end

  test "fails in domain initialization if the destination resource doesn't have the correct field" do
    assert_raise(
      Spark.Error.DslError,
      ~r/Relationship `post` expects source attribute `post_id` to be defined/,
      fn ->
        defposts do
          relationships do
            belongs_to(:post, __MODULE__, define_attribute?: false, public?: true)
          end
        end

        defmodule Domain do
          use Ash.Domain

          resources do
            resource Post
          end
        end
      end
    )
  end
end
