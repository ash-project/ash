defmodule Ash.Test.Dsl.Resource.Actions.ReadTest do
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
    test "it creates an action" do
      defposts do
        actions do
          read :default
        end
      end

      assert [
               %Ash.Resource.Actions.Read{
                 name: :default,
                 primary?: true,
                 authorization_steps: [],
                 type: :read
               }
             ] = Ash.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "action name must be an atom at actions -> read",
        fn ->
          defposts do
            actions do
              read "default"
            end
          end
        end
      )
    end

    test "it fails if `primary?` is not a boolean" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option primary? at actions -> read -> default must be boolean",
        fn ->
          defposts do
            actions do
              read :default, primary?: 10
            end
          end
        end
      )
    end

    test "it fails if `rules` is not a list" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option authorization_steps at actions -> read -> default must be [%Ash.Authorization.Rule{}]",
        fn ->
          defposts do
            actions do
              read :default, authorization_steps: 10
            end
          end
        end
      )
    end

    test "it fails if the elements of the rules list are not rules" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option authorization_steps at actions -> read -> default must be [%Ash.Authorization.Rule{}]",
        fn ->
          defposts do
            actions do
              read :default, authorization_steps: [10]
            end
          end
        end
      )
    end
  end
end
