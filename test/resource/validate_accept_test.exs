# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  test "accept :* is stored properly in action inputs along with arguments" do
    post =
      defposts do
        attributes do
          attribute :thing, :string, allow_nil?: false, public?: true
        end

        actions do
          create :example_action do
            accept :*
            argument :foo, :string, allow_nil?: false
          end
        end
      end

    inputs = Ash.Resource.Info.action_inputs(post, :example_action)
    assert :thing in inputs
    assert "thing" in inputs
    assert :foo in inputs
    assert "foo" in inputs
  end
end
