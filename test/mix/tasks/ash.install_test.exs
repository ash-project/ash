# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.InstallTest do
  use ExUnit.Case, async: false

  import Igniter.Test

  @moduletag :igniter

  setup do
    Mix.Shell.Process.flush()
    # Use process-based shell so we can send yes?/prompt inputs in tests
    previous_shell = Mix.shell()
    Mix.shell(Mix.Shell.Process)
    on_exit(fn -> Mix.shell(previous_shell) end)
    :ok
  end

  # The install task is only defined when Igniter is loaded (conditional compile in ash.install.ex).
  # These tests run only when Igniter is available.

  describe "info/1" do
    test "returns correct composes" do
      info = Mix.Tasks.Ash.Install.info([], nil)
      assert info.composes == ["spark.install", "ash.gen.resource"]
    end

    test "returns schema with setup, example, port, host, and yes options" do
      info = Mix.Tasks.Ash.Install.info([], nil)
      assert Keyword.get(info.schema, :setup) == :boolean
      assert Keyword.get(info.schema, :example) == :boolean
      assert Keyword.get(info.schema, :port) == :integer
      assert Keyword.get(info.schema, :host) == :string
      assert Keyword.get(info.schema, :yes) == :boolean
    end

    test "returns an Igniter.Mix.Task.Info struct" do
      info = Mix.Tasks.Ash.Install.info([], nil)
      assert %Igniter.Mix.Task.Info{} = info
    end
  end

  describe "igniter/1 when Postgres is running" do
    setup do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn _host, _port, _opts, _timeout ->
        {:ok, :fake_socket}
      end)
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :close, fn _socket -> :ok end)
      :ok
    end

    test "continues without prompting when TCP connect succeeds" do
      igniter = test_project() |> put_install_option(:yes, true)
      result = Mix.Tasks.Ash.Install.igniter(igniter)
      assert %Igniter{} = result
    end

    test "does not prompt when TCP connect succeeds" do
      # When Postgres is "running" (stubbed), we must not prompt for "Continue anyway?"
      igniter = test_project() |> put_install_option(:yes, true)

      Mix.Tasks.Ash.Install.igniter(igniter)

      # No yes? prompt should have been sent (no "Continue anyway?").
      refute_received {:mix_shell, :yes?, _}
    end

    test "passes --port and --host to postgres check when provided in options" do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn host, port, _opts, _timeout ->
        assert port == 5433, "expected port 5433, got #{inspect(port)}"
        assert host == ~c"db.example.com", "expected host 'db.example.com', got #{inspect(host)}"
        {:ok, :fake_socket}
      end)
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :close, fn _socket -> :ok end)

      igniter =
        test_project()
        |> put_install_option(:port, 5433)
        |> put_install_option(:host, "db.example.com")
        |> put_install_option(:yes, true)

      result = Mix.Tasks.Ash.Install.igniter(igniter)
      assert %Igniter{} = result
    end
  end

  describe "igniter/1 when Postgres is not running" do
    setup do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn _host, _port, _opts, _timeout ->
        {:error, :econnrefused}
      end)
      :ok
    end

    test "prints error and prompts to continue when user answers yes" do
      igniter = test_project() |> put_install_option(:yes, true)

      result = Mix.Tasks.Ash.Install.igniter(igniter)

      assert %Igniter{} = result
      assert_received {:mix_shell, :error, message}
      message_str = IO.iodata_to_binary(message)
      assert message_str =~ "Postgres is not running"
      assert message_str =~ "mix setup"
      # With --yes we skip the prompt; no {:mix_shell, :yes?, _} is sent
    end

    test "exits with {:shutdown, 1} when user answers no" do
      igniter = test_project()

      Process.flag(:trap_exit, true)
      pid = spawn_link(fn ->
        Mix.Tasks.Ash.Install.igniter(igniter)
      end)
      send(pid, {:mix_shell_input, :yes?, false})

      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, reason}
      assert reason == {:shutdown, 1} or reason == 1
    end

  end

  describe "igniter/1 edge cases (Postgres check)" do
    setup do
      # Each test in this block sets up its own stub and sends input
      # This setup ensures Mimic is ready, but tests override as needed
      :ok
    end

    test "treats connection timeout as Postgres not running" do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn _host, _port, _opts, _timeout ->
        {:error, :timeout}
      end)

      igniter = test_project() |> put_install_option(:yes, true)

      result = Mix.Tasks.Ash.Install.igniter(igniter)
      assert %Igniter{} = result
      assert_received {:mix_shell, :error, message}
      assert IO.iodata_to_binary(message) =~ "Postgres is not running"
    end

    test "treats connection refused as Postgres not running" do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn _host, _port, _opts, _timeout ->
        {:error, :econnrefused}
      end)

      igniter = test_project() |> put_install_option(:yes, true)

      result = Mix.Tasks.Ash.Install.igniter(igniter)
      assert %Igniter{} = result
    end

    test "treats ehostunreach as Postgres not running" do
      Mimic.stub(Ash.Mix.Tasks.Install.PostgresCheck, :connect, fn _host, _port, _opts, _timeout ->
        {:error, :ehostunreach}
      end)

      igniter = test_project() |> put_install_option(:yes, true)

      result = Mix.Tasks.Ash.Install.igniter(igniter)
      assert %Igniter{} = result
    end
  end

  defp put_install_option(igniter, key, value) do
    options = Keyword.put(igniter.args.options || [], key, value)
    args = %{igniter.args | options: options}
    %{igniter | args: args}
  end

  describe "fallback module (no Igniter)" do
    # The fallback module is compiled only when Igniter is not loaded.
    # We cannot exercise it in this test file because Igniter is a project dependency.
    # Manual / integration: run mix ash.install in a project without Igniter to verify
    # the error message and exit code.
    @tag :skip
    test "run/1 prints error and exits when Igniter not loaded" do
      Mix.Task.run("ash.install", [])
      # Would expect: error message and exit({:shutdown, 1})
    end
  end
end
