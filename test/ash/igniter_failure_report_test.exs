# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.IgniterFailureReportTest do
  @moduledoc """
  Tests for Ash.Igniter failure reporting: un-run tasks and failures are properly logged.

  Verifies:
  - pending_task_entries/1: nil/empty tasks, string-only task, {name, args}, delayed, order
  - format_pending_tasks/1: empty log, single/multiple tasks, args, special chars, header/bullets
  - run_with_failure_report/2: on failure invokes on_failure_log with correct content and re-raises

  Uses test/fixtures/ash_igniter_failure_test_project/ as a real Mix project for tests
  that need a project directory (e.g. running igniter from fixture path).
  """
  use ExUnit.Case

  import Igniter.Test

  @moduletag :igniter

  defp fixture_path do
    # Resolve at runtime so it works from project root (mix test) or test/ash
    Path.join([File.cwd!(), "test", "fixtures", "ash_igniter_failure_test_project"])
    |> Path.expand()
  end

  describe "pending_task_entries/1 edge cases" do
    test "nil tasks list is treated as empty" do
      igniter = test_project()
      igniter = %{igniter | tasks: nil}
      assert Ash.Igniter.pending_task_entries(igniter) == []
    end

    test "empty tasks list returns empty list" do
      igniter = test_project()
      assert Ash.Igniter.pending_task_entries(igniter) == []
    end

    test "task as plain string (name only) returns {name, []}" do
      igniter = test_project() |> Igniter.add_task("ash.setup", [])
      # Ensure we have the string-only form; add_task may store {name, args}
      igniter = %{igniter | tasks: ["single.task.only"]}
      assert Ash.Igniter.pending_task_entries(igniter) == [{"single.task.only", []}]
    end

    test "task with args returns {name, args}" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.codegen", ["my_name"])

      assert Ash.Igniter.pending_task_entries(igniter) == [{"ash.codegen", ["my_name"]}]
    end

    test "delayed task includes :delayed in args" do
      igniter =
        test_project()
        |> Igniter.add_task("first", [])
        |> Igniter.delay_task("delayed.task")

      entries = Ash.Igniter.pending_task_entries(igniter)
      assert {"first", []} in entries
      assert {"delayed.task", [":delayed"]} in entries
    end

    test "order of tasks is preserved" do
      igniter =
        test_project()
        |> Igniter.add_task("task.one", [])
        |> Igniter.add_task("task.two", ["a"])
        |> Igniter.add_task("task.three", [])

      assert Ash.Igniter.pending_task_entries(igniter) == [
               {"task.one", []},
               {"task.two", ["a"]},
               {"task.three", []}
             ]
    end
  end

  describe "format_pending_tasks/1 edge cases" do
    test "empty task list produces 'No tasks were queued.'" do
      igniter = test_project()
      assert Ash.Igniter.format_pending_tasks(igniter) == "No tasks were queued."
    end

    test "nil tasks produce 'No tasks were queued.'" do
      igniter = %{test_project() | tasks: nil}
      assert Ash.Igniter.format_pending_tasks(igniter) == "No tasks were queued."
    end

    test "single task without args has header and one bullet" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])

      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "Tasks that did not run (or may not have completed):"
      assert log =~ "  • ash.setup"
    end

    test "multiple tasks with args are all listed with args" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])
        |> Igniter.add_task("ash.codegen", ["foo", "bar"])

      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "Tasks that did not run"
      assert log =~ "  • ash.setup"
      assert log =~ "  • ash.codegen foo bar"
    end

    test "delayed task appears with :delayed in formatted line" do
      igniter =
        test_project()
        |> Igniter.delay_task("ash.setup")

      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "ash.setup"
      assert log =~ ":delayed"
    end

    test "args with special characters are preserved" do
      igniter = %{test_project() | tasks: [{"some.task", ["--flag=value", "path/with/slash"]}]}
      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "some.task --flag=value path/with/slash"
    end
  end

  describe "run_with_failure_report/2" do
    test "on failure calls on_failure_log with formatted pending tasks and re-raises" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])
        |> Igniter.add_task("ash.codegen", ["foo"])

      log_ref = make_ref()
      parent = self()

      assert_raise ArgumentError, fn ->
        Ash.Igniter.run_with_failure_report(igniter,
          dry_run: true,
          on_failure_log: fn log ->
            send(parent, {:failure_log, log_ref, log})
          end
        )
      end

      assert_received {:failure_log, ^log_ref, log}
      assert log =~ "Tasks that did not run (or may not have completed):"
      assert log =~ "  • ash.setup"
      assert log =~ "  • ash.codegen foo"
    end

    test "on failure with empty task list still invokes on_failure_log with 'No tasks were queued.'" do
      igniter = test_project()
      log_ref = make_ref()
      parent = self()

      assert_raise ArgumentError, fn ->
        Ash.Igniter.run_with_failure_report(igniter,
          dry_run: true,
          on_failure_log: fn log ->
            send(parent, {:failure_log, log_ref, log})
          end
        )
      end

      assert_received {:failure_log, ^log_ref, log}
      assert log =~ "No tasks were queued."
    end
  end

  describe "with fixture project" do
    @tag :fixture_project
    test "format_pending_tasks produces correct log when igniter built from fixture path" do
      path = fixture_path()
      File.cd!(path, fn ->
        igniter =
          test_project()
          |> Igniter.add_task("ash.setup", [])
          |> Igniter.add_task("ash.codegen", ["example"])

        log = Ash.Igniter.format_pending_tasks(igniter)
        assert log =~ "Tasks that did not run"
        assert log =~ "  • ash.setup"
        assert log =~ "  • ash.codegen example"
      end)
    end

    @tag :fixture_project
    test "run_with_failure_report logs pending tasks on failure when run from fixture project" do
      path = fixture_path()
      File.cd!(path, fn ->
        igniter =
          test_project()
          |> Igniter.add_task("ash.setup", [])

        log_ref = make_ref()
        parent = self()

        try do
          Ash.Igniter.run_with_failure_report(igniter,
            dry_run: true,
            on_failure_log: fn log ->
              send(parent, {:failure_log, log_ref, log})
            end
          )
        rescue
          _ -> :ok
        end

        assert_received {:failure_log, ^log_ref, log}
        assert log =~ "Tasks that did not run"
        assert log =~ "  • ash.setup"
      end)
    end
  end
end
