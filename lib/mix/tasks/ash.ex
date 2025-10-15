# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash do
  # Copied from Phoenix
  # https://github.com/phoenixframework/phoenix/blob/v1.8.0-rc.3/lib/mix/tasks/phx.ex#L38
  use Mix.Task

  @shortdoc "Prints Ash help information"

  @moduledoc """
  Prints Ash tasks and their information.

      $ mix ash

  To print the Ash version, pass `-v` or `--version`, for example:

      $ mix ash --version

  """

  @version Mix.Project.config()[:version]

  @impl true
  @doc false
  def run([version]) when version in ~w(-v --version) do
    Mix.shell().info("Ash v#{@version}")
  end

  def run(args) do
    case args do
      [] -> general()
      _ -> Mix.raise("Invalid arguments, expected: mix ash")
    end
  end

  defp general do
    Application.ensure_all_started(:ash)
    Mix.shell().info("Ash v#{Application.spec(:ash, :vsn)}")
    Mix.shell().info("Model your domain, derive the rest")
    Mix.shell().info("\n## Options\n")
    Mix.shell().info("-v, --version        # Prints Ash version\n")
    Mix.Tasks.Help.run(["--search", "ash."])
  end
end
