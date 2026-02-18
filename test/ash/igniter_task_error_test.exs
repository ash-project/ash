# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT
#
# Tests that Ash.Igniter.compile_task_error_message/4 and task_error_report/3
# return all expected failure information (failed task, tasks not run, message).

if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Igniter.TaskErrorTest do
    @moduledoc false
    use ExUnit.Case, async: true

    alias Ash.Igniter

    describe "compile_task_error_message/4" do
      test "returns message with failed task and all subsequent tasks as not run" do
        igniter = %{tasks: [{"ash.codegen", ["foo"]}, {"ash.other", []}, {"mix.something", ["arg"]}]}
        error = %RuntimeError{message: "compile failed"}

        msg = Igniter.compile_task_error_message(igniter, 0, error)

        assert msg =~ "Task 1 (ash.codegen [\"foo\"]) failed: compile failed"
        assert msg =~ "The following tasks were not run:"
        assert msg =~ "ash.other []"
        assert msg =~ "mix.something [\"arg\"]"
      end

      test "when last task fails: message has no 'tasks were not run' list" do
        igniter = %{tasks: [{"ash.codegen", ["x"]}, {"ash.other", []}]}
        error = %RuntimeError{message: "codegen crashed"}

        msg = Igniter.compile_task_error_message(igniter, 1, error)

        assert msg =~ "Task 2 (ash.other []) failed: codegen crashed"
        refute msg =~ "The following tasks were not run"
        assert msg =~ "failed: codegen crashed."
      end

      test "when middle task fails: only tasks after it are listed as not run" do
        igniter = %{tasks: [{"first", []}, {"second", []}, {"third", []}]}
        error = %RuntimeError{message: "second failed"}

        msg = Igniter.compile_task_error_message(igniter, 1, error)

        assert msg =~ "Task 2 (second []) failed: second failed"
        assert msg =~ "The following task was not run: third []"
      end

      test "single task not run uses singular 'task was not run'" do
        igniter = %{tasks: [{"a", []}, {"b", []}]}
        msg = Igniter.compile_task_error_message(igniter, 0, "err")

        assert msg =~ "The following task was not run:"
        assert msg =~ "b []"
      end

      test "multiple tasks not run use plural 'tasks were not run'" do
        igniter = %{tasks: [{"a", []}, {"b", []}, {"c", []}]}
        msg = Igniter.compile_task_error_message(igniter, 0, "err")

        assert msg =~ "The following tasks were not run:"
      end

      test "format :iodata returns iodata that converts to same string" do
        igniter = %{tasks: [{"ash.codegen", ["x"]}, {"other", []}]}
        error = %RuntimeError{message: "fail"}

        str = Igniter.compile_task_error_message(igniter, 0, error, format: :string)
        iodata = Igniter.compile_task_error_message(igniter, 0, error, format: :iodata)

        assert IO.iodata_to_binary(iodata) == str
      end

      test "empty tasks: failed task shown as (unknown), no not-run list" do
        igniter = %{tasks: []}
        msg = Igniter.compile_task_error_message(igniter, 0, "error")

        assert msg =~ "Task 1 ((unknown) []) failed:"
        refute msg =~ "The following"
      end

      test "igniter without :tasks key: treated as empty, no crash" do
        igniter = %{}
        msg = Igniter.compile_task_error_message(igniter, 0, "err")

        assert msg =~ "Task 1 ((unknown) []) failed:"
      end

      test "error as struct: message uses Exception.message" do
        igniter = %{tasks: [{"task", []}]}
        error = %ArgumentError{message: "invalid argument"}

        msg = Igniter.compile_task_error_message(igniter, 0, error)

        assert msg =~ "failed: invalid argument"
      end

      test "error as non-struct term: message uses inspect" do
        igniter = %{tasks: [{"task", []}]}
        error = {:error, :timeout}

        msg = Igniter.compile_task_error_message(igniter, 0, error)

        assert msg =~ "failed: {:error, :timeout}"
      end
    end

    describe "task_error_report/3" do
      test "returns all expected keys" do
        igniter = %{tasks: [{"ash.codegen", ["a"]}, {"ash.other", []}]}
        error = %RuntimeError{message: "fail"}

        report = Igniter.task_error_report(igniter, 0, error)

        assert Map.has_key?(report, :failed_at_index)
        assert Map.has_key?(report, :failed_task)
        assert Map.has_key?(report, :tasks_not_run)
        assert Map.has_key?(report, :task_count)
        assert Map.has_key?(report, :error)
        assert Map.has_key?(report, :message)
      end

      test "failed_at_index matches input" do
        igniter = %{tasks: [{"a", []}, {"b", []}, {"c", []}]}
        report = Igniter.task_error_report(igniter, 1, "err")

        assert report.failed_at_index == 1
      end

      test "failed_task is the task tuple at failed index" do
        igniter = %{tasks: [{"first", []}, {"second", ["x"]}, {"third", []}]}
        report = Igniter.task_error_report(igniter, 1, "err")

        assert report.failed_task == {"second", ["x"]}
      end

      test "tasks_not_run lists only tasks after failed index" do
        igniter = %{tasks: [{"one", []}, {"two", []}, {"three", []}]}
        report = Igniter.task_error_report(igniter, 0, "err")

        assert report.tasks_not_run == ["two []", "three []"]
      end

      test "tasks_not_run is empty when last task fails" do
        igniter = %{tasks: [{"a", []}, {"b", []}]}
        report = Igniter.task_error_report(igniter, 1, "err")

        assert report.tasks_not_run == []
      end

      test "task_count is total number of tasks" do
        igniter = %{tasks: [{"a", []}, {"b", []}, {"c", []}]}
        report = Igniter.task_error_report(igniter, 1, "err")

        assert report.task_count == 3
      end

      test "error is the same term passed in" do
        igniter = %{tasks: [{"t", []}]}
        error = %RuntimeError{message: "same"}
        report = Igniter.task_error_report(igniter, 0, error)

        assert report.error == error
      end

      test "message matches compile_task_error_message output" do
        igniter = %{tasks: [{"ash.codegen", ["x"]}, {"other", []}]}
        error = %RuntimeError{message: "match"}

        expected = Igniter.compile_task_error_message(igniter, 0, error)
        report = Igniter.task_error_report(igniter, 0, error)

        assert report.message == expected
      end

      test "invalid index: failed_task is nil, tasks_not_run empty" do
        igniter = %{tasks: [{"a", []}]}
        report = Igniter.task_error_report(igniter, 5, "err")

        assert report.failed_task == nil
        assert report.tasks_not_run == []
        assert report.task_count == 1
        assert report.message =~ "(unknown)"
      end

      test "negative index: tasks_not_run empty" do
        igniter = %{tasks: [{"a", []}, {"b", []}]}
        report = Igniter.task_error_report(igniter, -1, "err")

        assert report.tasks_not_run == []
      end
    end

    describe "expected failure content" do
      test "all failures include which task failed (position and name)" do
        igniter = %{tasks: [{"ash.codegen", ["res"]}, {"ash.setup", []}]}
        msg = Igniter.compile_task_error_message(igniter, 0, "err")

        assert msg =~ "Task 1"
        assert msg =~ "ash.codegen"
        assert msg =~ "[\"res\"]"
      end

      test "all failures include the error reason" do
        igniter = %{tasks: [{"t", []}]}
        msg = Igniter.compile_task_error_message(igniter, 0, "custom reason")

        assert msg =~ "custom reason"
      end

      test "when there are skipped tasks, all are listed in report and message" do
        igniter = %{tasks: [{"a", []}, {"b", [1, 2]}, {"c", []}]}
        report = Igniter.task_error_report(igniter, 0, "x")
        msg = report.message

        assert report.tasks_not_run == ["b [1, 2]", "c []"]
        assert msg =~ "b [1, 2]"
        assert msg =~ "c []"
      end
    end
  end
end
