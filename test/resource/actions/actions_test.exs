defmodule Ash.Test.Dsl.Resource.Actions.ActionsTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post", primary_key: false

        unquote(body)
      end
    end
  end

  describe "validations" do
    test "raises if you have multiple primary actions for a type" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "Multiple actions of type create configured as `primary?: true`, but only one action per type can be the primary at actions -> create",
        fn ->
          defposts do
            actions do
              create :default, primary?: true
              create :special, primary?: true
            end
          end
        end
      )
    end

    test "raises if you have multiple actions for a type, but none are primary" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "Multiple actions of type create defined, one must be designated as `primary?: true` at actions -> create",
        fn ->
          defposts do
            actions do
              create :default
              create :special
            end
          end
        end
      )
    end
  end
end
