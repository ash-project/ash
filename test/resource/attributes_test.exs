defmodule Ash.Test.Resource.AttributesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, name: "posts", type: "post"

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
        "attributes -> attribute:\n  Attribute name must be an atom, got: 10",
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
        "attributes -> attribute -> foo:\n  Attribute type must be a built in type or a type module, got: 10",
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
        "attributes -> attribute:\n  expected :primary_key? to be an boolean, got: 10",
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
                 generated?: false,
                 name: :updated_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: ^default,
                 writable?: false
               },
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 generated?: false,
                 name: :inserted_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: nil,
                 writable?: false
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
                 name: :last_visited,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: ^default,
                 writable?: false
               },
               %Ash.Resource.Attributes.Attribute{
                 allow_nil?: true,
                 default: ^default,
                 name: :created_at,
                 primary_key?: false,
                 type: :utc_datetime,
                 update_default: nil,
                 writable?: false
               }
             ] = Ash.attributes(Post)
    end
  end
end
