defmodule Ash.Test.Resource.VerifyFilterExpressionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers
  alias Spark.Error.DslError

  test "Filter expression with undefined argument raises an error" do
    assert_raise DslError,
                 ~r/Filter expression references undefined argument `undefined_arg`/,
                 fn ->
                   defposts do
                     actions do
                       read :example_action do
                         argument :valid_arg, :string
                         filter expr(some_field == ^arg(:undefined_arg))
                       end
                     end
                   end
                 end
  end

  test "Filter expression with valid argument does not raise an error" do
    post =
      defposts do
        actions do
          read :example_action do
            argument :valid_arg, :string
            filter expr(some_field == ^arg(:valid_arg))
          end
        end
      end

    action = Ash.Resource.Info.action(post, :example_action)
    assert action.name == :example_action
  end

  test "Filter expression with valid string argument does not raise an error" do
    post =
      defposts do
        actions do
          read :example_action do
            argument :valid_arg, :string
            filter expr(some_field == ^arg("valid_arg"))
          end
        end
      end

    action = Ash.Resource.Info.action(post, :example_action)
    assert action.name == :example_action
  end
end
