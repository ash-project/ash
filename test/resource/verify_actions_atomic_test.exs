# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.VerifyActionsAtomicTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Helpers

  test "an atomic-capable validation marked before_transaction? warns on require_atomic? actions" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          attributes do
            attribute :name, :string, public?: true
          end

          actions do
            default_accept :*

            update :atomic_update do
              require_atomic? true
              validate attribute_equals(:name, "fred"), before_transaction?: true
            end
          end
        end
      end)

    assert output =~ "cannot be done atomically"
  end

  test "an atomic-capable validation without before_transaction? does not warn" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defposts do
          attributes do
            attribute :name, :string, public?: true
          end

          actions do
            default_accept :*

            update :atomic_update do
              require_atomic? true
              validate attribute_equals(:name, "fred")
            end
          end
        end
      end)

    refute output =~ "cannot be done atomically"
  end
end
