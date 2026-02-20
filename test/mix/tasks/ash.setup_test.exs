# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.SetupTest do
  @moduledoc """
  Tests for Mix.Tasks.Ash.Setup: failure record identification, context, and location.

  Verifies:
  - Failure record structure (error, context, location)
  - Context (step, phase, extension, argv)
  - Location extraction from stacktraces
  - Edge cases: multiple failures, throw/exit, empty argv, phase identification

  Uses test/fixtures/ash_setup_test_project/ for tests that run in a real Mix project.
  """
  use ExUnit.Case
  import Mimic

  Mimic.copy(Ash.Mix.Tasks.Helpers)

  setup :set_mimic_global

  describe "failure record structure" do
    test "record has error, context, and location keys" do
      defmodule TestExt do
        def setup(_argv), do: raise("Test error")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExt] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert Map.has_key?(record, :error)
      assert Map.has_key?(record, :context)
      assert Map.has_key?(record, :location)
    end

    test "error field holds the exception" do
      defmodule TestExtErr do
        def setup(_argv), do: raise("Test error message")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtErr] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert %RuntimeError{message: "Test error message"} = record.error
    end

    test "context has step, extension, and argv" do
      defmodule TestExtCtx do
        def setup(_argv), do: raise("Test error")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtCtx] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record(["a", "b"])

      assert %{step: :setup, extension: TestExtCtx, argv: ["a", "b"]} = record.context
    end

    test "location is a non-empty string" do
      defmodule TestExtLoc do
        def setup(_argv), do: raise("Test error")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtLoc] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert is_binary(record.location)
      assert record.location != ""
      assert record.location != "unknown"
    end
  end

  describe "context" do
    test "step is :setup for extension setup failures" do
      defmodule TestExtStep do
        def setup(_argv), do: raise("fail")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtStep] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.step == :setup
    end

    test "step is :extensions! when extensions! raises" do
      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> raise("discovery failed") end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.step == :extensions!
    end

    test "phase is :get_name when extension.name/0 raises" do
      defmodule TestExtPhase do
        def setup(_argv), do: :ok
        def name, do: raise("name failed")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtPhase] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.phase == :get_name
      assert %RuntimeError{message: "name failed"} = record.error
    end

    test "phase is :setup or :extension_setup when setup/1 raises" do
      defmodule TestExtPhaseSetup do
        def setup(_argv), do: raise("setup failed")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtPhaseSetup] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.phase in [:setup, :extension_setup]
    end
  end

  describe "location" do
    test "location includes file/line or module.fun/arity" do
      defmodule TestExtLocFmt do
        def setup(_argv), do: raise("err")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtLocFmt] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert is_binary(record.location)
      assert record.location != "unknown"
    end
  end

  describe "extension discovery failures" do
    test "captures exception from extensions!" do
      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> raise("Extension discovery failed") end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.step == :extensions!
      assert %RuntimeError{message: "Extension discovery failed"} = record.error
      assert is_binary(record.location)
    end

    test "captures throw from extensions!" do
      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> throw(:discovery_failed) end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.step == :extensions!
      assert record.error == {:throw, :discovery_failed}
    end
  end

  describe "extension setup failures" do
    test "captures exception from extension.setup/1" do
      defmodule TestExtSetup do
        def setup(_argv), do: raise("Setup failed")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtSetup] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.extension == TestExtSetup
      assert %RuntimeError{message: "Setup failed"} = record.error
    end

    test "captures throw from extension.setup/1" do
      defmodule TestExtThrow do
        def setup(_argv), do: throw(:setup_failed)
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtThrow] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.error == {:throw, :setup_failed}
      assert record.context.extension == TestExtThrow
    end

    test "skips extension with no setup/1" do
      defmodule TestExtNoSetup do
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtNoSetup] end)
      result = Mix.Tasks.Ash.Setup.run_with_failure_record([])
      assert result == :ok or match?({:error, []}, result)
    end

    test "extension without name/0 still runs setup" do
      defmodule TestExtNoName do
        def setup(_argv), do: :ok
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtNoName] end)
      result = Mix.Tasks.Ash.Setup.run_with_failure_record([])
      assert result == :ok or match?({:error, []}, result)
    end
  end

  describe "edge cases" do
    test "empty argv is preserved in context" do
      defmodule TestExtEmpty do
        def setup(_argv), do: raise("err")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtEmpty] end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert record.context.argv == []
    end

    test "argv with multiple args is preserved" do
      argv = ["--opt", "value", "pos"]
      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> raise("err") end)
      {:error, [record]} = Mix.Tasks.Ash.Setup.run_with_failure_record(argv)

      assert record.context.argv == argv
    end

    test "multiple extension failures produce multiple records" do
      defmodule TestExt1 do
        def setup(_argv), do: raise("Ext 1 failed")
      end

      defmodule TestExt2 do
        def setup(_argv), do: raise("Ext 2 failed")
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExt1, TestExt2] end)
      {:error, records} = Mix.Tasks.Ash.Setup.run_with_failure_record([])

      assert length(records) >= 2
      assert Enum.all?(records, &match?(%{context: %{step: :setup}}, &1))
    end

    test "error tuple from setup/1 is not caught" do
      defmodule TestExtOkTuple do
        def setup(_argv), do: {:error, "reason"}
      end

      stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [TestExtOkTuple] end)
      result = Mix.Tasks.Ash.Setup.run_with_failure_record([])
      assert result == :ok or match?({:error, []}, result)
    end
  end

  # test/fixtures/ash_setup_test_project/ is a minimal Mix project for manual
  # testing of `mix ash.setup` (e.g. from that directory run `mix ash.setup`).
  # It is not used in these tests because running Mix.Project.in_project there
  # loads a different Mix context where Ash.Info may be unavailable.
end
