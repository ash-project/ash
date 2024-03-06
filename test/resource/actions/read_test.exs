defmodule Ash.Test.Dsl.Resource.Actions.ReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource,
          domain: Domain,
          data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "it creates an action" do
      defposts do
        actions do
          default_accept :*
          read :read
        end
      end

      assert [
               %Ash.Resource.Actions.Read{
                 name: :read,
                 primary?: false,
                 type: :read
               }
             ] = Ash.Resource.Info.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :name option: expected atom, got: "default"/,
        fn ->
          defposts do
            actions do
              default_accept :*
              read "default"
            end
          end
        end
      )
    end

    test "it fails if `primary?` is not a boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :primary\? option: expected boolean, got: 10/,
        fn ->
          defposts do
            actions do
              default_accept :*
              read :read, primary?: 10
            end
          end
        end
      )
    end
  end
end
