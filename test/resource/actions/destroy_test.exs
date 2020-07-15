defmodule Ash.Test.Dsl.Resource.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "it creates an action" do
      defposts do
        actions do
          destroy :default
        end
      end

      assert [
               %Ash.Resource.Actions.Destroy{
                 name: :default,
                 primary?: true,
                 type: :destroy
               }
             ] = Ash.Resource.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Ash.Error.ResourceDslError,
        "actions -> destroy -> default:\n  expected :name to be an atom, got: \"default\"",
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
        Ash.Error.ResourceDslError,
        "actions -> destroy -> default:\n  expected :primary? to be an boolean, got: 10",
        fn ->
          defposts do
            actions do
              destroy :default, primary?: 10
            end
          end
        end
      )
    end
  end
end
