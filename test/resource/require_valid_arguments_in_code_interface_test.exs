defmodule Resource.RequireValidArgumentsInCodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "fails if one of the code_interface arguments is not a valid attribute or argument" do
    assert_raise(
      Spark.Error.DslError,
      ~r/Cannot accept the args `\[:oops\]` because they are not arguments or attributes supported by the `:read` action/,
      fn ->
        defposts do
          code_interface do
            define :read, args: [:oops, :bar]
          end

          attributes do
            attribute :foo, :string
          end

          actions do
            read :read do
              argument :bar, :string
            end
          end
        end
      end
    )
  end

  test "fails if one of the arguments is not an accepted attribute" do
    assert_raise(
      Spark.Error.DslError,
      ~r/Cannot accept the attributes `\[:foo\]` as args because they are not defined in the `accept` list of the `:update` action/,
      fn ->
        defposts do
          code_interface do
            define :update, args: [:foo, :bar]
          end

          attributes do
            attribute :foo, :string
          end

          actions do
            update :update do
              argument :bar, :string
              # Do not accept :foo
              accept []
            end
          end
        end
      end
    )
  end

  test "passes if the arguments are valid" do
    defposts do
      code_interface do
        define :read, args: [:bar]
        define :update, args: [:foo, :bar]
      end

      attributes do
        attribute :foo, :string
      end

      actions do
        read :read do
          argument :bar, :string
        end

        update :update do
          argument :bar, :string
          accept [:foo]
        end
      end
    end
  end
end
