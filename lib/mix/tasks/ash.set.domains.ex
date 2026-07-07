# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Set.Domains do
  use Igniter.Mix.Task

  @shortdoc "Dynamically discovers and updates Ash domains in config.exs"

  @moduledoc """
  Scans your application layout for Ash domains and automatically updates the
  `:ash_domains` configuration array inside `config/config.exs`.

  ## Example

      $ mix ash.set.domains
  """

  @impl Igniter.Mix.Task
  def igniter(igniter, _argv) do
    app_name = Igniter.Project.Application.app_name(igniter)

    Mix.shell().info("🔍 Scanning for Ash domains in :#{app_name}...")

    {igniter, domains} = Ash.Mix.Tasks.Helpers.discover_domains(igniter)

    if Enum.empty?(domains) do
      Mix.shell().error("❌ No modules utilizing `use Ash.Domain` were found.")
      igniter
    else
      Mix.shell().info("✨ Found #{length(domains)} domain(s). Patching configuration...")

      igniter
      |> Igniter.Project.Config.configure(
        "config.exs",
        app_name,
        [:ash_domains],
        domains
      )
    end
  end
end
