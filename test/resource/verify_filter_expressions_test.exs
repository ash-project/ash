# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.VerifyFilterExpressionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "Filter expression with undefined argument raises an error" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          actions do
            read :example_action do
              argument :valid_arg, :string
              filter expr(some_field == ^arg(:undefined_arg))
            end
          end
        end
      end)

    assert String.contains?(output, "Filter expression references undefined argument")
    assert String.contains?(output, "undefined_arg")
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
