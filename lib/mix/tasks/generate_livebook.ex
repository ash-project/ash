defmodule Mix.Tasks.Ash.GenerateLivebook do
  @moduledoc """
  Generates a Livebook for each Ash API.

  ## Command line options

    * `--only` - only generates the given API file

  """
  use Mix.Task

  @shortdoc "Generates a Livebook for each Ash API"
  def run(_argv) do
    Mix.Task.run("compile")

    File.write!("livebook.livemd", Ash.Api.Info.Livebook.overview(apis()))

    Mix.shell().info("Generated Livebook")
  end

  def apis do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_apis, [])
  end
end
