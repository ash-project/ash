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
    end

    assert Ash.Resource.Info.primary_action!(Post, :read)
  end

  describe "validations" do
    test "raises if you have multiple primary actions for a type" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Transformers.SetPrimaryActions]\n actions -> create:\n  Multiple actions of type create configured as `primary?: true`, but only one action per type can be the primary",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Resource.Transformers.SetPrimaryActions]\n actions -> create:\n  Multiple actions of type create defined, one must be designated as `primary?: true`",
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
