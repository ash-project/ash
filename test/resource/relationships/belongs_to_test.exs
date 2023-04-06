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

  defmacrop defcomments(do: body) do
    quote do
      defmodule Comment do
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

    test "it creates an attribute that honors attribute_writable?" do
      defposts do
        relationships do
          belongs_to(:foobar, FooBar, attribute_writable?: true)
        end
      end

      assert [
               _,
               %Ash.Resource.Attribute{
                 name: :foobar_id,
                 primary_key?: false,
                 type: Ash.Type.UUID,
                 writable?: true
               }
             ] = Ash.Resource.Info.attributes(Post)
    end

    test "it creates a relationship" do
      defposts do
        relationships do
          belongs_to(:foo, Foo)
          belongs_to(:bar, Bar, source_attribute: :bazz, private?: true)
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
                 private?: false
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
    test "fails if destination_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :destination_attribute option: expected atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, destination_attribute: "foo")
            end
          end
        end
      )
    end

    test "fails if source_attribute is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :source_attribute option: expected atom, got: \"foo\"",
        fn ->
          defposts do
            relationships do
              belongs_to(:foobar, FooBar, source_attribute: "foo")
            end
          end
        end
      )
    end

    test "fails if the destination is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :destination option: expected atom, got: \"foobar\"",
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
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :name option: expected atom, got: \"foobar\"",
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
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :primary_key? option: expected boolean, got: \"blah\"",
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
        Spark.Error.DslError,
        "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :private? option: expected boolean, got: \"blah\"",
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

  test "fails if `define_attribute?` is not a boolean" do
    assert_raise(
      Spark.Error.DslError,
      "[Ash.Test.Resource.Relationships.BelongsToTest.Post]\n relationships -> belongs_to -> foobar:\n  invalid value for :define_attribute? option: expected boolean, got: \"blah\"",
      fn ->
        defposts do
          relationships do
            belongs_to(:foobar, Foobar, define_attribute?: "blah")
          end
        end
      end
    )
  end

  test "fails in api initialization if the destination resource doesn't have the correct field" do
    assert_raise(
      Spark.Error.DslError,
      ~r/Relationship `post` expects source field `post_id` to be defined/,
      fn ->
        defposts do
          relationships do
            belongs_to(:post, __MODULE__, define_attribute?: false)
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

  test "don't add required error for belongs_to if other errors are present" do
    defposts do
      actions do
        defaults [:create, :read, :update, :destroy]
      end

      relationships do
        has_many(:comments, Comment)
      end
    end

    defcomments do
      actions do
        defaults [:create]
      end

      relationships do
        belongs_to(:post, Post, allow_nil?: false)
      end
    end

    defmodule Registry do
      @moduledoc false
      use Ash.Registry

      entries do
        entry(Post)
        entry(Comment)
      end
    end

    defmodule Api do
      @moduledoc false
      use Ash.Api

      resources do
        registry Registry
      end
    end

    assert {:error,
            %Ash.Error.Invalid{
              changeset: %{
                errors: [
                  # there should not be a Ash.Error.Changes.Required error in this list
                  %Ash.Error.Invalid{path: [:post]}
                ]
              }
            }} =
             Comment
             |> Ash.Changeset.new()
             |> Ash.Changeset.manage_relationship(:post, Ecto.UUID.generate(),
               type: :append_and_remove
             )
             |> Api.create()
  end
end
