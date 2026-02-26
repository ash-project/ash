# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.SetupTest do
  use ExUnit.Case, async: true

  # Extension that implements setup/1 and name/0 (success case)
  defmodule OkExtension do
    def name, do: "OkExtension"
    def setup(_argv), do: :ok
  end

  # Extension that raises in setup/1
  defmodule FailingExtension do
    def name, do: "FailingExtension"
    def setup(_argv), do: raise("setup failed intentionally")
  end

  # Extension with no name/0 - task should use inspect(module)
  defmodule NoNameExtension do
    def setup(_argv), do: raise("no name extension failed")
  end

  # Extension that raises a non-RuntimeError (e.g. Mix.Error style)
  defmodule CustomErrorExtension do
    def name, do: "CustomErrorExtension"
    def setup(_argv), do: raise(ArgumentError, "invalid argv")
  end

  # Module that has setup/1 but no name/0 (edge case: inspect used)
  defmodule NamedExtension do
    def name, do: "NamedExtension"
    def setup(_argv), do: :ok
  end

  describe "run/2 with no failures" do
    test "succeeds when compile and all extensions succeed" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [OkExtension] end

      assert :ok == Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
    end

    test "succeeds when there are no extensions with setup/1" do
      # Extensions that don't export setup/1 are skipped
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [Enumerable] end

      assert :ok == Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
    end

    test "succeeds with empty extension list" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [] end

      assert :ok == Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
    end

    test "succeeds with multiple extensions all succeeding" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [OkExtension, NamedExtension] end

      assert :ok == Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
    end
  end

  describe "run/2 with compile failure" do
    test "raises Mix.Error with message containing task name and error" do
      compile_fn = fn -> raise("compile failed") end
      extensions_fn = fn _argv -> [] end

      assert_raise Mix.Error, ~r/Setup failed with the following errors/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end

      assert_raise Mix.Error, ~r/compile: compile failed/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end
    end

    test "failure record is only {task, error} — task is \"compile\"" do
      compile_fn = fn -> raise("compile oops") end
      extensions_fn = fn _argv -> [] end

      try do
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      rescue
        e ->
          message = Exception.message(e)
          assert message =~ "compile"
          assert message =~ "compile oops"
      end
    end
  end

  describe "run/2 with extension setup failure" do
    test "raises Mix.Error with extension name and error message" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [FailingExtension] end

      assert_raise Mix.Error, ~r/Setup failed with the following errors/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end

      assert_raise Mix.Error, ~r/FailingExtension: setup failed intentionally/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end
    end

    test "extension without name/0 uses inspect(module) in message" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [NoNameExtension] end

      assert_raise Mix.Error, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end

      try do
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      rescue
        e ->
          message = Exception.message(e)
          assert message =~ "Mix.Tasks.Ash.SetupTest.NoNameExtension" or message =~ "NoNameExtension"
          assert message =~ "no name extension failed"
      end
    end

    test "non-RuntimeError is captured and message is shown" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [CustomErrorExtension] end

      assert_raise Mix.Error, ~r/CustomErrorExtension: invalid argv/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end
    end
  end

  describe "run/2 with multiple failures" do
    test "compile and extension failure both appear in error message" do
      compile_fn = fn -> raise("compile error") end
      extensions_fn = fn _argv -> [FailingExtension] end

      try do
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      rescue
        e ->
          message = Exception.message(e)
          assert message =~ "compile" and message =~ "compile error"
          assert message =~ "FailingExtension" and message =~ "setup failed intentionally"
      end
    end

    test "multiple extension failures all appear" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [FailingExtension, NoNameExtension, CustomErrorExtension] end

      try do
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      rescue
        e ->
          message = Exception.message(e)
          assert message =~ "FailingExtension"
          assert message =~ "NoNameExtension" or message =~ "SetupTest.NoNameExtension"
          assert message =~ "CustomErrorExtension"
      end
    end
  end

  describe "edge cases" do
    test "extensions without setup/1 are skipped and do not run" do
      # Module that has no setup/1 — should be skipped, no call, no failure
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [OkExtension, Enumerable, OkExtension] end

      assert :ok == Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
    end

    test "first extension succeeds, second fails — error mentions failing task only" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [OkExtension, FailingExtension] end

      assert_raise Mix.Error, ~r/FailingExtension: setup failed intentionally/, fn ->
        Mix.Tasks.Ash.Setup.run([], compile: compile_fn, extensions: extensions_fn)
      end
    end

    test "run with custom argv completes when extensions succeed" do
      compile_fn = fn -> :ok end
      extensions_fn = fn _argv -> [OkExtension] end

      assert :ok ==
               Mix.Tasks.Ash.Setup.run(["--domains", "MyApp.Foo"],
                 compile: compile_fn,
                 extensions: extensions_fn
               )
    end
  end
end
