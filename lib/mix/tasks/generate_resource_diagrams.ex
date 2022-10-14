defmodule Mix.Tasks.Ash.GenerateResourceDiagrams do
  @moduledoc """
  Generates a Mermaid Resource Diagram for each Ash API.

  ## Prerequisites

  This mix task requires the Mermaid CLI to be installed on your system.

  See https://github.com/mermaid-js/mermaid-cli

  ## Command line options

    * `--type` - `er` or `class` (defaults to `class`)
    * `--only` - only generates the given API file

  """
  use Mix.Task

  @shortdoc "Generates Mermaid Resource Diagrams for each Ash API"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [only: :keep, type: :string],
        aliases: [o: :only, t: :type]
      )

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      end

    apis()
    |> Task.async_stream(
      fn api ->
        source = api.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          case Keyword.get(opts, :type, "class") do
            "er" ->
              source
              |> Mix.Mermaid.file("mermaid-er-diagram", "pdf")
              |> Mix.Mermaid.create_diagram(Ash.Api.Info.Diagram.mermaid_er_diagram(api))

              Mix.shell().info("Generated ER Diagram for #{inspect(api)}")

            "class" ->
              source
              |> Mix.Mermaid.file("mermaid-class-diagram", "pdf")
              |> Mix.Mermaid.create_diagram(Ash.Api.Info.Diagram.mermaid_class_diagram(api))

              Mix.shell().info("Generated Class Diagram for #{inspect(api)}")

            type ->
              Mix.shell().error("""
              Invalid resource diagram type `#{type}`.

              Valid options are `er` or `class`.
              """)
          end
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp apis do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_apis, [])
  end
end
