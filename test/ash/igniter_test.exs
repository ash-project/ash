# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.IgniterTest do
  use ExUnit.Case

  import Igniter.Test

  @moduletag :igniter

  describe "pending_task_entries/1" do
    test "returns name and empty args for task name only" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.codegen", [])

      assert Ash.Igniter.pending_task_entries(igniter) == [{"ash.codegen", []}]
    end

    test "returns name and args for task with args" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])
        |> Igniter.add_task("ash.codegen", ["my_name"])

      assert Ash.Igniter.pending_task_entries(igniter) == [
               {"ash.setup", []},
               {"ash.codegen", ["my_name"]}
             ]
    end

    test "includes :delayed in args for delayed tasks" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.codegen", [])
        |> Igniter.delay_task("ash.setup")

      entries = Ash.Igniter.pending_task_entries(igniter)
      assert {"ash.codegen", []} in entries
      assert {"ash.setup", [":delayed"]} in entries
    end
  end

  describe "format_pending_tasks/1" do
    test "formats empty task list" do
      igniter = test_project()
      assert Ash.Igniter.format_pending_tasks(igniter) == "No tasks were queued."
    end

    test "formats single task without args" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])

      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "Tasks that did not run"
      assert log =~ "• ash.setup"
    end

    test "formats multiple tasks with args" do
      igniter =
        test_project()
        |> Igniter.add_task("ash.setup", [])
        |> Igniter.add_task("ash.codegen", ["foo"])

      log = Ash.Igniter.format_pending_tasks(igniter)
      assert log =~ "Tasks that did not run"
      assert log =~ "• ash.setup"
      assert log =~ "• ash.codegen foo"
    end
  end

end
