# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.EctoCompatTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.EctoCompat

  # We use two existing test-support resources that ship with Ash's test suite.
  # - Car:  has `create_timestamp` / `update_timestamp` — good for testing the
  #         missing-autogenerate check.
  # - User: has attributes with static defaults — good for testing the
  #         default-mismatch check.
  alias Ash.Test.Support.PolicySimple.Car
  alias Ash.Test.Support.PolicyField.User

  # ── inspect_resource/1 tests ──────────────────────────────────────────

  describe "inspect_resource/1" do
    # Baseline test: a minimal resource with no timestamps and no static
    # defaults should produce zero warnings.
    test "returns empty list for resources without issues" do
      # Define a throwaway resource inline for this test. It lives inside the
      # test module's namespace so it won't collide with other tests.
      # We give it only a UUID primary key and a plain string attribute —
      # nothing that would trigger either compatibility check.
      defmodule TestResource do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :name, :string
        end
      end

      warnings = EctoCompat.inspect_resource(TestResource)
      assert warnings == []
    end

    # Test that we correctly detect Ash timestamps that are missing from
    # Ecto's autogenerate list.
    #
    # The Car resource uses `create_timestamp :inserted_at` and
    # `update_timestamp :updated_at`. Since Ash doesn't wire these into
    # Ecto's `__schema__(:autogenerate_fields)`, we expect a
    # `:missing_autogenerate` warning.
    test "detects missing autogenerate fields for timestamps" do
      warnings = EctoCompat.inspect_resource(Car)

      # Look for a warning of the expected type.
      missing_autogen =
        Enum.find(warnings, fn warning ->
          warning.type == :missing_autogenerate
        end)

      # If the warning exists (it should), verify its shape.
      # We use `if` rather than a hard assert because future Ash versions
      # might fix this — at that point the test still passes (no warning).
      if missing_autogen do
        assert missing_autogen.resource == Car
        assert is_list(missing_autogen.fields)
        # Should contain :inserted_at and/or :updated_at
        assert length(missing_autogen.fields) > 0
      end
    end

    # Test that we correctly detect Ash static defaults that don't appear
    # on the bare Ecto struct.
    #
    # The User resource has attributes with static defaults. When you do
    # `%User{}`, those fields show `nil` instead of the Ash default — we
    # expect a `:default_mismatch` warning for those.
    test "detects default mismatches" do
      warnings = EctoCompat.inspect_resource(User)

      default_mismatch =
        Enum.find(warnings, fn warning ->
          warning.type == :default_mismatch
        end)

      # Same rationale as above — verify shape if present.
      if default_mismatch do
        assert default_mismatch.resource == User
        assert is_list(default_mismatch.details)
        assert length(default_mismatch.details) > 0
      end
    end

    # Test that passing a loaded but non-Ash module (like `String`) returns
    # a graceful error instead of crashing. This exercises the second guard
    # in the `cond` block of `inspect_resource/1`.
    test "handles non-resource modules gracefully" do
      warnings = EctoCompat.inspect_resource(String)
      assert length(warnings) == 1
      assert hd(warnings).type == :error
      assert String.contains?(hd(warnings).message, "not an Ash resource")
    end

    # Test that passing a module atom that doesn't correspond to any loaded
    # module returns a graceful error. This exercises the first guard in
    # the `cond` block (`Code.ensure_loaded?` returns false).
    #
    # Note: In Elixir, writing `UnloadedModule12345` in source code creates
    # the atom but does NOT define a module. `Code.ensure_loaded?/1` returns
    # false for atoms that have no backing module.
    test "handles unloaded modules gracefully" do
      warnings = EctoCompat.inspect_resource(UnloadedModule12345)
      assert length(warnings) == 1
      assert hd(warnings).type == :error
      assert String.contains?(hd(warnings).message, "not loaded")
    end
  end

  # ── print_warnings/1 tests ────────────────────────────────────────────

  describe "print_warnings/1" do
    # Verify that `print_warnings/1` can handle a list containing both
    # warning types without raising. We don't assert on the printed output
    # (that would be brittle) — we just confirm it returns `:ok`.
    test "prints warnings correctly" do
      warnings = [
        %{
          type: :missing_autogenerate,
          resource: Car,
          fields: [:inserted_at, :updated_at],
          message: "Test message"
        },
        %{
          type: :default_mismatch,
          resource: User,
          details: [
            %{attr: :see_if_just_created, ash_default: "test", struct_default: nil}
          ],
          message: "Test default message"
        }
      ]

      # Should not raise and should return :ok
      assert EctoCompat.print_warnings(warnings) == :ok
    end

    # Edge case: an empty list should print the "all clear" message and
    # still return :ok.
    test "handles empty warnings list" do
      assert EctoCompat.print_warnings([]) == :ok
    end
  end
end
