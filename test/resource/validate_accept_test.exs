defmodule Ash.Test.Resource.ValidateAcceptTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers
  alias Spark.Error.DslError

  test "Accepting an attribute that does not exist raises an error" do
    assert_raise DslError, ~r/\[:invalid\], because they are not attributes/, fn ->
      defposts do
        actions do
          default_accept :*
          create :example_action, accept: [:invalid]
        end
      end
    end
  end
end
