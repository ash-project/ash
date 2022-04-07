defmodule Ash.Test.Resource.RequireUniqueActionNamesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "fails if there are multiple read actions" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      ~r/Multiple actions \(2\) with the name `read` defined in/,
      fn ->
        defposts do
          actions do
            defaults [:read]
            read :read
          end
        end
      end
    )
  end

  test "passes if there is one default read action" do
    defposts do
      actions do
        defaults [:read]
      end
    end
  end

  test "passes if there is only one defined action" do
    defposts do
      actions do
        read :read
      end
    end
  end
end
