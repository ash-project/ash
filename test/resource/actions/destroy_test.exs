defmodule Ash.Test.Dsl.Resource.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource,
          data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates an action" do
      defposts do
        actions do
          destroy :destroy
        end
      end

      assert [
               %Ash.Resource.Actions.Destroy{
                 name: :destroy,
                 primary?: false,
                 type: :destroy
               }
             ] = Ash.Resource.Info.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Dsl.Resource.Actions.DestroyTest.Post]\n actions -> destroy -> default:\n  invalid value for :name option: expected atom, got: \"default\"",
        fn ->
          defposts do
            actions do
              destroy "default"
            end
          end
        end
      )
    end

    test "it fails if `primary?` is not a boolean" do
      assert_raise(
        Spark.Error.DslError,
        "[Ash.Test.Dsl.Resource.Actions.DestroyTest.Post]\n actions -> destroy -> destroy:\n  invalid value for :primary? option: expected boolean, got: 10",
        fn ->
          defposts do
            actions do
              destroy :destroy, primary?: 10
            end
          end
        end
      )
    end
  end
end
