# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.VerifyPublicActions do
  @shortdoc "Runs tests that verify the public? / private_action? implementation"
  @moduledoc """
  Runs the test suite that verifies the Ash public? action option and
  private_action?() policy check implementation (internal-only actions feature).

  Use this after implementing the feature to confirm:
  - Policy tests pass (including private_action? usage).
  - Resource/Info tests pass (public_actions/1, action structs with public?).
  - No regressions in related action/policy behavior.

  Exits with code 0 only if all verification tests pass.
  """

  use Mix.Task

  @impl true
  def run(_args) do
    Mix.Task.run("compile", ["--force"])

    test_paths = [
      "test/policy/",
      "test/resource/info_test.exs",
      "test/resource/actions/actions_test.exs"
    ]

    failures =
      for path <- test_paths do
        if run_test(path), do: nil, else: path
      end
      |> Enum.reject(&is_nil/1)

    if failures == [] do
      Mix.shell().info("")
      Mix.shell().info([:green, "All public? / private_action? verification tests passed."])
    else
      Mix.shell().error("")

      Mix.shell().error([
        :red,
        "Verification failed for: " <> Enum.join(failures, ", ")
      ])

      System.halt(1)
    end
  end

  defp run_test(path) do
    Mix.shell().info("Running: mix test #{path}")
    {_output, status} = System.cmd("mix", ["test", path], into: IO.stream(:stdio, :line))
    status == 0
  end
end
