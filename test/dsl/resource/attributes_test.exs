defmodule Ash.Test.Dsl.Resource.AttributesTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post"

        unquote(body)
      end
    end
  end

  describe "validation" do
    test "raises if the attribute name is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "Ash.Test.Dsl.Resource.AttributesTest.Post: Attribute name must be an atom, got: 10 at attributes->attribute",
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
        "Ash.Test.Dsl.Resource.AttributesTest.Post: Attribute type must be a built in type or a type module, got: 10 at attributes->attribute",
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
        "Ash.Test.Dsl.Resource.AttributesTest.Post: option primary_key? at attributes->attribute must be of type :boolean",
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
end
