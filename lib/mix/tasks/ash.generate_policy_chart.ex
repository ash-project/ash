defmodule Mix.Tasks.Ash.GeneratePolicyCharts do
  @moduledoc """
  Generates a Mermaid Flow Chart for a given resource's policies.

  ## Prerequisites

  This mix task requires the Mermaid CLI to be installed on your system.

  See https://github.com/mermaid-js/mermaid-cli

  ## Command line options

    * `--only` - only generates the given Flow file
    * `--format` - Can be set to one of either:
      * `plain` - Prints just the mermaid output as text. This is the default.
      * `md` - Prints the mermaid diagram in a markdown code block.
      * `svg` - Generates an SVG
      * `pdf` - Generates a PDF
      * `png` - Generates a PNG
  """
  use Mix.Task

  @recursive true

  @shortdoc "Generates a Mermaid Flow Chart for a given resource's policies."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [only: :keep, all: :boolean, format: :string],
        aliases: [o: :only, f: :format, a: :all]
      )

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      else
        if !opts[:all] do
          raise "Must pass the `--only` option or the `--all` option."
        end
      end

    format = Keyword.get(opts, :format, "plain")

    resources()
    |> Stream.filter(fn resource ->
      Ash.Policy.Authorizer in Spark.extensions(resource)
    end)
    |> Task.async_stream(
      fn resource ->
        source = resource.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          Mix.Mermaid.generate_diagram(
            source,
            "policy-flowchart",
            format,
            Ash.Policy.Chart.Mermaid.chart(resource),
            "Generated Mermaid Flow Chart for #{inspect(resource)}"
          )
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp resources do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_domains, [])
    |> Enum.flat_map(&Ash.Domain.Info.resources/1)
  end
end
