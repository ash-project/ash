defmodule Mix.Tasks.Ash.GenerateClassDiagrams do
  @moduledoc """
  Generates a Mermaid Class Diagram for each Ash API.

  ## Prerequisites

  This mix task requires the Mermaid CLI to be installed on your system.

  See https://github.com/mermaid-js/mermaid-cli

  ## Command line options

    * `--only` - only generates the given API file

  """
  use Mix.Task

  @shortdoc "Generates Mermaid Class Diagrams for each Ash API"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} = OptionParser.parse!(argv, strict: [only: :keep], aliases: [o: :only])

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      end

    apis()
    |> Task.async_stream(
      fn api ->
        source = api.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          make_diagram(api, Mix.Mermaid.file(source, "mermaid-class-diagram", "pdf"))

          Mix.shell().info("Generated Class Diagram for #{inspect(api)}")
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp make_diagram(api, file) do
    Mix.Mermaid.create_diagram(file, Ash.Api.Info.Diagram.mermaid_class_diagram(api))
  end

  def apis do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_apis, [])
  end
end
