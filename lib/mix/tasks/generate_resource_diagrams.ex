defmodule Mix.Tasks.Ash.GenerateResourceDiagrams do
  @moduledoc """
  Generates a Mermaid Resource Diagram for each Ash domain.

  ## Prerequisites

  This mix task requires the Mermaid CLI to be installed on your system.

  See https://github.com/mermaid-js/mermaid-cli

  ## Command line options

    * `--type` - `er` or `class` (defaults to `class`)
    * `--only` - only generates for the given domain
    * `--format` - Can be set to one of either:
      * `plain` - Prints just the mermaid output as text. This is the default.
      * `md` - Prints the mermaid diagram in a markdown code block.
      * `svg` - Generates an SVG
      * `pdf` - Generates a PDF
      * `png` - Generates a PNG

  """
  use Mix.Task

  @recursive true

  @shortdoc "Generates Mermaid Resource Diagrams for each Ash domain"
  @doc @shortdoc
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

    domains()
    |> Task.async_stream(
      fn domain ->
        source = domain.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          case Keyword.get(opts, :type, "class") do
            "er" ->
              Mix.Mermaid.generate_diagram(
                source,
                "mermaid-er-diagram",
                format,
                Ash.Domain.Info.Diagram.mermaid_er_diagram(domain),
                "Generated ER Diagram for #{inspect(domain)}"
              )

            "class" ->
              Mix.Mermaid.generate_diagram(
                source,
                "mermaid-class-diagram",
                format,
                Ash.Domain.Info.Diagram.mermaid_class_diagram(domain),
                "Generated Class Diagram for #{inspect(domain)}"
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

  defp domains do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_domains, [])
  end
end
