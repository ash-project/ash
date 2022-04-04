defmodule Ash.Test.Dsl.Resource.Actions.CreateTest do
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
          create :create
        end
      end

      assert [
               %Ash.Resource.Actions.Create{
                 name: :create,
                 primary?: true,
                 type: :create
               }
             ] = Ash.Resource.Info.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Dsl.Resource.Actions.CreateTest.Post]\n actions -> create -> default:\n  expected :name to be an atom, got: \"default\"",
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
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Dsl.Resource.Actions.CreateTest.Post]\n actions -> create -> create:\n  expected :primary? to be a boolean, got: 10",
        fn ->
          defposts do
            actions do
              create :create, primary?: 10
            end
          end
        end
      )
    end
  end
end
