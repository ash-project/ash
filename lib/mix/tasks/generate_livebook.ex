defmodule Mix.Tasks.Ash.GenerateLivebook do
  @moduledoc """
  Generates a Livebook for each Ash domain.

  ## Command line options

    * `--filename` - Specify the name of the generated Livebook file. Default: `livebook.livemd`

  """
  use Mix.Task

  @shortdoc "Generates a Livebook for each Ash domain"
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [filename: :string],
        aliases: [f: :filename]
      )

    filename = Keyword.get(opts, :filename, "livebook.livemd")

    File.write!(filename, Ash.Domain.Info.Livebook.overview(domains()))

    Mix.shell().info("Generated Livebook")
  end

  def domains do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_domains, [])
  end
end
