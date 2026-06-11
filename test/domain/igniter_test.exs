# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Domain.IgniterTest do
  # async: false - list_domains reads Application.get_env at runtime,
  # which is global state that leaks between concurrent tests.
  use ExUnit.Case, async: false

  import Igniter.Test

  # ---------------------------------------------------------------------------
  # Test 1 - plain module gets upgraded in-place.
  # ---------------------------------------------------------------------------
  describe "add_resource_reference/3 when domain module exists but is not an Ash.Domain" do
    test "does NOT emit the old confusing warning" do
      igniter =
        test_project(
          files: %{
            "lib/my_app/billing.ex" => """
            defmodule MyApp.Billing do
              # Plain module - not an Ash.Domain
            end
            """
          }
        )
        |> Ash.Domain.Igniter.add_resource_reference(MyApp.Billing, MyApp.Billing.Invoice)

      refute Enum.any?(igniter.warnings, &String.contains?(&1, "was not an `Ash.Domain`"))
    end

    test "patches the domain file to include the resource reference" do
      {:ok, igniter, _} =
        test_project(
          files: %{
            "lib/my_app/billing.ex" => """
            defmodule MyApp.Billing do
              # Plain module - not an Ash.Domain
            end
            """
          }
        )
        |> Ash.Domain.Igniter.add_resource_reference(MyApp.Billing, MyApp.Billing.Invoice)
        |> apply_igniter()

      source = Rewrite.source!(igniter.rewrite, "lib/my_app/billing.ex")
      content = Rewrite.Source.get(source, :content)
      assert String.contains?(content, "MyApp.Billing.Invoice")
    end
  end

  # ---------------------------------------------------------------------------
  # Test 2 - fallback: domain module does NOT exist -> original warning preserved.
  # ---------------------------------------------------------------------------
  describe "add_resource_reference/3 when domain module does not exist" do
    test "emits the original warning" do
      test_project()
      |> Ash.Domain.Igniter.add_resource_reference(MyApp.Nonexistent, MyApp.Nonexistent.Thing)
      |> assert_has_warning(
        "Domain MyApp.Nonexistent was not an `Ash.Domain`, so could not add `MyApp.Nonexistent.Thing` to its resource list."
      )
    end

    test "does not patch any files" do
      test_project()
      |> Ash.Domain.Igniter.add_resource_reference(MyApp.Nonexistent, MyApp.Nonexistent.Thing)
      |> assert_unchanged()
    end
  end
end
