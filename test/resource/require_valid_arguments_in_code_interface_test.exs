# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Resource.RequireValidArgumentsInCodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "fails if one of the code_interface arguments is not a valid attribute or argument" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
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
      end)

    assert String.contains?(output, "Cannot accept the args") and
             String.contains?(output, ":oops")
  end

  test "fails if one of the arguments is not an accepted attribute" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
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
      end)

    assert String.contains?(output, "Cannot accept the attributes") and
             String.contains?(output, ":foo")
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
