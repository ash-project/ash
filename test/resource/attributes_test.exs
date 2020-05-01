defmodule Ash.Test.Resource.AttributesTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post", primary_key: false

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "attributes are persisted on the resource properly" do
      defposts do
        attributes do
          attribute :foo, :string
        end
      end

      assert [%Ash.Resource.Attributes.Attribute{name: :foo, type: :string, primary_key?: false}] =
               Ash.attributes(Post)
    end
  end

  describe "validation" do
    test "raises if the attribute name is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "Attribute name must be an atom, got: 10 at attributes -> attribute",
        fn ->
          defposts do
            attributes do
              attribute 10, :string
            end
          end
        end
      )
    end

    test "raises if the type is not a known type" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "Attribute type must be a built in type or a type module, got: 10 at attributes -> attribute -> foo",
        fn ->
          defposts do
            attributes do
              attribute :foo, 10
            end
          end
        end
      )
    end

    test "raises if you pass an invalid value for `primary_key?`" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option primary_key? at attributes -> attribute must be boolean",
        fn ->
          defposts do
            attributes do
              attribute :foo, :string, primary_key?: 10
            end
          end
        end
      )
    end
  end

  describe "timestamps" do
    test "it adds utc_datetime attributes" do
      defposts do
        attributes do
          timestamps()
        end
      end

      default = &DateTime.utc_now/0

      assert [
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 generated?: true,
                 name: :updated_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: ^default,
                 writable?: true,
                 write_rules: []
               },
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 generated?: true,
                 name: :inserted_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: nil,
                 writable?: true,
                 write_rules: []
               }
             ] = Ash.attributes(Post)
    end

    test "it allows overwriting the field names" do
      defposts do
        attributes do
          timestamps(inserted_at_field: :created_at, updated_at_field: :last_visited)
        end
      end

      default = &DateTime.utc_now/0

      assert [
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 generated?: true,
                 name: :last_visited,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: ^default,
                 writable?: true,
                 write_rules: []
               },
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 generated?: true,
                 name: :created_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: nil,
                 writable?: true,
                 write_rules: []
               }
             ] = Ash.attributes(Post)
    end
  end
end
