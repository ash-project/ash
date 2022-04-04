defmodule Ash.Test.Dsl.Resource.Actions.ActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end
    end
  end

  test "default actions are added" do
    defposts do
      actions do
        defaults [:create, :read, :update, :destroy]
      end
    end

    assert Ash.Resource.Info.primary_action!(Post, :read)
    assert Ash.Resource.Info.primary_action!(Post, :create)
    assert Ash.Resource.Info.primary_action!(Post, :update)
    assert Ash.Resource.Info.primary_action!(Post, :destroy)
  end

  describe "validations" do
    test "raises if you have multiple primary actions for a type" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Dsl.Resource.Actions.ActionsTest.Post]\n actions -> create:\n  Multiple actions of type create configured as `primary?: true`, but only one action per type can be the primary",
        fn ->
          defposts do
            actions do
              create :create, primary?: true
              create :special, primary?: true
            end
          end
        end
      )
    end
  end
end
