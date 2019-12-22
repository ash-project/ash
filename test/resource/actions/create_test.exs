defmodule Ash.Test.Dsl.Resource.Actions.CreateTest do
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        use Ash.Resource, name: "posts", type: "post"

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates an action" do
      defposts do
        actions do
          create :default
        end
      end

      assert [
               %Ash.Resource.Actions.Create{
                 name: :default,
                 primary?: true,
                 authorization_steps: [],
                 type: :create
               }
             ] = Ash.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "action name must be an atom at actions -> create",
        fn ->
          defposts do
            actions do
              create "default"
            end
          end
        end
      )
    end

    test "it fails if `primary?` is not a boolean" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option primary? at actions -> create -> default must be boolean",
        fn ->
          defposts do
            actions do
              create :default, primary?: 10
            end
          end
        end
      )
    end

    test "it fails if `rules` is not a list" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "option authorization_steps at actions -> create -> default must be [any]",
        fn ->
          defposts do
            actions do
              create :default, authorization_steps: 10
            end
          end
        end
      )
    end
  end
end
