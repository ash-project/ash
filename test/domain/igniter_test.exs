defmodule Ash.Domain.IgniterTest do
  use ExUnit.Case, async: true

  import Igniter.Test

  # Helpers

  defp plain_module_source do
    """
    defmodule MyApp.Billing do
      # Plain module — not an Ash.Domain
    end
    """
  end

  # Test 1 — plain module gets upgraded in-place.
  describe "add_resource_reference/3 when domain module exists but is not an Ash.Domain" do
    test "does NOT emit the old confusing warning" do
      igniter =
        test_project(
          files: %{
            "lib/my_app/billing.ex" => plain_module_source()
          }
        )
        |> Ash.Domain.Igniter.add_resource_reference(MyApp.Billing, MyApp.Billing.Invoice)

      refute Enum.any?(igniter.warnings, &String.contains?(&1, "was not an `Ash.Domain`"))
    end

    test "patches the domain file to add the resources block and reference" do
      test_project(
        files: %{
          "lib/my_app/billing.ex" => plain_module_source()
        }
      )
      |> Ash.Domain.Igniter.add_resource_reference(MyApp.Billing, MyApp.Billing.Invoice)
      |> assert_has_patch("lib/my_app/billing.ex", """
           1 + |defmodule MyApp.Billing do
           2 + |  resources do
           3 + |    resource(MyApp.Billing.Invoice)
           4 + |  end
           5 + |
           6 + |  # Plain module — not an Ash.Domain
      """)
    end
  end


  # Test 2 — fallback: domain module does NOT exist → original warning preserved.

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
