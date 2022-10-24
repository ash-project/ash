defmodule Mix.Tasks.Ash.GenerateResourceDiagrams do
  @moduledoc """
  Generates a Mermaid Resource Diagram for each Ash API.

  ## Prerequisites

  This mix task requires the Mermaid CLI to be installed on your system.

  See https://github.com/mermaid-js/mermaid-cli

  ## Command line options

    * `--type` - `er` or `class` (defaults to `class`)
    * `--only` - only generates the given API file
    * `--format` - Can be set to one of either:
      * `plain` - Prints just the mermaid output as text. This is the default.
      * `md` - Prints the mermaid diagram in a markdown code block.
      * `svg` - Generates an SVG
      * `pdf` - Generates a PDF
      * `png` - Generates a PNG

  """
  use Mix.Task

  @shortdoc "Generates Mermaid Resource Diagrams for each Ash API"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [only: :keep, type: :string, format: :string],
        aliases: [o: :only, t: :type, f: :format]
      )

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      end

    format = Keyword.get(opts, :format, "plain")

    apis()
    |> Task.async_stream(
      fn api ->
        source = api.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          case Keyword.get(opts, :type, "class") do
            "er" ->
              Mix.Mermaid.generate_diagram(
                source,
                "mermaid-er-diagram",
                format,
                Ash.Api.Info.Diagram.mermaid_er_diagram(api),
                "Generated ER Diagram for #{inspect(api)}"
              )

            "class" ->
              Mix.Mermaid.generate_diagram(
                source,
                "mermaid-class-diagram",
                format,
                Ash.Api.Info.Diagram.mermaid_class_diagram(api),
                "Generated Class Diagram for #{inspect(api)}"
              )

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
