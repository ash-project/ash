# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.SetupTest do
  use ExUnit.Case, async: false

  alias Mix.Tasks.Ash.Setup

  # Path to the fixture mix project used by integration tests (same folder as this test)
  @fixture_project_path Path.expand("ash_setup_project", __DIR__)

  # Minimal Mix.Project for testing - pushed before each test
  defmodule TestMixProject do
    def project do
      [
        app: :test_ash_setup,
        version: "0.1.0",
        elixir: "~> 1.11"
      ]
    end
  end

  setup do
    # Push a minimal Mix.Project so Mix.Project.config() and Mix.Project.apps_paths() work
    Mix.Project.push(TestMixProject)
    on_exit(fn -> Mix.Project.pop() end)
    :ok
  end

  # Fake extension that raises in setup/1 – used to trigger extension setup failure records
  defmodule FakeExtensionSetupFails do
    def name, do: "FakeExtension"
    def setup(_argv), do: raise("setup failed intentionally")
  end

  # Extension that raises in name/0 – used to trigger name failure records
  defmodule FakeExtensionNameFails do
    def name, do: raise("name failed intentionally")
    def setup(_argv), do: :ok
  end

  # Extension with no setup/1 – should be skipped (no record)
  defmodule FakeExtensionNoSetup do
    def name, do: "NoSetup"
  end

  # Extension that succeeds – for mixed success/failure tests
  defmodule FakeExtensionSucceeds do
    def name, do: "Succeeds"
    def setup(_argv), do: :ok
  end

  describe "run_with_failure_record/1" do
    test "returns :ok or {:error, list} and does not raise" do
      # Stub compile to succeed so Mix.Project is not required
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      result = Setup.run_with_failure_record([])
      assert result == :ok or (is_tuple(result) and elem(result, 0) == :error)
      if is_tuple(result) and elem(result, 0) == :error do
        records = elem(result, 1)
        assert is_list(records)
        Enum.each(records, &assert_valid_failure_record/1)
      end
    end

    test "when compile fails, returns single error record with correct context and location" do
      Mimic.copy(Mix.Task)

      Mimic.stub(Mix.Task, :run, fn
        "compile" -> raise "simulated compile failure"
        _name -> []
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1, "expected exactly one failure record for compile failure"

      [record] = records
      assert_valid_failure_record(record)
      assert record.context =~ "compile", "context should identify compile step"
      assert record.context =~ "Mix.Task.run"
      assert record.location =~ "compile"
      assert record.location =~ "Mix.Tasks.Ash.Setup.run"
      assert record.error != nil
      assert is_binary(Exception.message(record.error)) or is_exception(record.error)
    end

    test "when extensions! raises, returns single error record identifying extensions step" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _argv ->
        raise "extensions! failed (e.g. option parse or domains load)"
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1

      [record] = records
      assert_valid_failure_record(record)
      assert record.context =~ "extensions!"
      assert record.context =~ "extension modules"
      assert record.location =~ "extensions!(argv)"
      assert record.error != nil
    end

    test "when extension.setup/1 raises, returns record identifying setup and extension" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSetupFails]
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1

      [record] = records
      assert_valid_failure_record(record)
      assert record.context =~ "extension.setup(argv)"
      assert record.context =~ "FakeExtension"
      assert record.location =~ "extension.setup(argv)"
      assert record.location =~ "FakeExtensionSetupFails"
      assert record.error != nil
      assert Exception.message(record.error) =~ "setup failed intentionally"
    end

    test "when extension.name/0 raises, returns record identifying name failure" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionNameFails]
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1

      [record] = records
      assert_valid_failure_record(record)
      assert record.context =~ "name"
      assert record.context =~ "display name"
      assert record.location =~ "name for"
      assert record.location =~ "FakeExtensionNameFails"
      assert record.error != nil
      assert Exception.message(record.error) =~ "name failed intentionally"
    end

    test "extension with no setup/1 is skipped and produces no record" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionNoSetup]
      end)

      # Should succeed: no setup/1 means run_extension_setup returns []
      result = Setup.run_with_failure_record([])
      assert result == :ok
    end

    test "multiple extension failures are all reported in records" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSetupFails, FakeExtensionNameFails]
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 2, "expected one record per failing extension"

      [r1, r2] = records
      assert_valid_failure_record(r1)
      assert_valid_failure_record(r2)

      contexts = [r1.context, r2.context]
      assert Enum.any?(contexts, &(&1 =~ "setup")), "one record should be for setup"
      assert Enum.any?(contexts, &(&1 =~ "name")), "one record should be for name"

      locations = [r1.location, r2.location]
      assert Enum.any?(locations, &(&1 =~ "FakeExtensionSetupFails"))
      assert Enum.any?(locations, &(&1 =~ "FakeExtensionNameFails"))
    end

    test "mixed success and failure extensions: only failures appear in records" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSucceeds, FakeExtensionSetupFails, FakeExtensionSucceeds]
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1, "only the failing extension should produce a record"

      [record] = records
      assert record.context =~ "FakeExtension"
      assert record.location =~ "FakeExtensionSetupFails"
    end
  end

  describe "failure record structure and content" do
    test "every failure record has required keys and meaningful values" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSetupFails]
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      Enum.with_index(records, 1) |> Enum.each(fn {record, i} ->
        assert Map.has_key?(record, :error), "record #{i} missing :error"
        assert Map.has_key?(record, :context), "record #{i} missing :context"
        assert Map.has_key?(record, :location), "record #{i} missing :location"
        assert is_binary(record.context) and byte_size(record.context) > 0
        assert is_binary(record.location) and byte_size(record.location) > 0
      end)
    end

    test "location identifies compile step when compile fails" do
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> raise "x"
        _name -> []
      end)

      assert {:error, [r]} = Setup.run_with_failure_record([])
      assert r.location =~ "Mix.Task.run"
    end

    test "location identifies extensions! step when extensions! fails" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> raise "y" end)

      assert {:error, [r]} = Setup.run_with_failure_record([])
      assert r.location =~ "extensions!(argv)"
    end

    test "location identifies extension.setup step when extension setup fails" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [FakeExtensionSetupFails] end)

      assert {:error, [r]} = Setup.run_with_failure_record([])
      assert r.location =~ "extension.setup(argv)"
      assert r.location =~ "FakeExtensionSetupFails"
    end
  end

  describe "run/1 raises with formatted failure message" do
    test "run/1 raises when there are failures and message contains locations and context" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSetupFails]
      end)

      assert_raise Mix.Error, ~r/Ash\.Setup failed/, fn ->
        Setup.run([])
      end

      try do
        Setup.run([])
      rescue
        e in Mix.Error ->
          msg = Exception.message(e)
          assert msg =~ "Ash.Setup failed"
          assert msg =~ "extension.setup(argv)"
          assert msg =~ "FakeExtension"
          assert msg =~ "FakeExtensionSetupFails"
          assert msg =~ "setup failed intentionally"
      end
    end

    test "struct exception message appears in formatted output" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ ->
        [FakeExtensionSetupFails]
      end)

      try do
        Setup.run([])
      rescue
        e in Mix.Error ->
          msg = Exception.message(e)
          assert msg =~ "error:"
          assert msg =~ "setup failed intentionally"
      end
    end
  end

  describe "edge cases" do
    test "record preserves non-RuntimeError exception type" do
      defmodule RaiseArgumentError do
        def name, do: "Arg"
        def setup(_), do: raise(ArgumentError, "bad arg")
      end

      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> [RaiseArgumentError] end)

      assert {:error, [record]} = Setup.run_with_failure_record([])
      assert_valid_failure_record(record)
      assert %ArgumentError{} = record.error
      assert Exception.message(record.error) =~ "bad arg"
    end

    test "first failure stops later steps (compile failure prevents extensions! run)" do
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> raise "compile died"
        _n -> []
      end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1
      assert hd(records).context =~ "compile"
      # extensions! is never called when compile fails, so we get only one record
    end

    test "extensions! failure returns error immediately with single record" do
      # Stub compile to succeed
      Mimic.copy(Mix.Task)
      Mimic.stub(Mix.Task, :run, fn
        "compile" -> []
        _name -> []
      end)

      Mimic.copy(Ash.Mix.Tasks.Helpers)
      Mimic.stub(Ash.Mix.Tasks.Helpers, :extensions!, fn _ -> raise "domains load failed" end)

      assert {:error, records} = Setup.run_with_failure_record([])
      assert length(records) == 1
      assert hd(records).context =~ "extensions!"
    end
  end

  describe "integration with fixture project" do
    @moduletag :integration

    setup do
      # Ensure fixture project has deps and compiles before running ash.setup
      {_, 0} = System.cmd("mix", ["deps.get"], cd: @fixture_project_path, env: [{"MIX_ENV", "test"}])
      {_, 0} = System.cmd("mix", ["compile"], cd: @fixture_project_path, env: [{"MIX_ENV", "test"}])
      :ok
    end

    test "mix ash.setup runs successfully in fixture project" do
      {output, exit_status} =
        System.cmd("mix", ["ash.setup"], cd: @fixture_project_path, env: [{"MIX_ENV", "test"}])

      assert exit_status == 0,
             "Expected mix ash.setup to succeed in fixture project. Exit status: #{exit_status}, output: #{inspect(output)}"
    end
  end

  defp assert_valid_failure_record(record) do
    assert is_map(record), "failure record must be a map"
    assert Map.has_key?(record, :error), "record must have :error"
    assert Map.has_key?(record, :context), "record must have :context"
    assert Map.has_key?(record, :location), "record must have :location"
    assert record.context != nil and is_binary(record.context)
    assert record.location != nil and is_binary(record.location)
  end
end
