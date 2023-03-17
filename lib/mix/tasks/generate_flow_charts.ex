defmodule Mix.Tasks.Ash.GenerateFlowCharts do
  @moduledoc """
  Generates a Mermaid Flow Chart for each `Ash.Flow` alongside the flow.

  If there is a `run_flow` step in the flow, this will also create
  an "expanded" Mermaid Flow Chart which includes all child steps.

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

  @shortdoc "Generates Mermaid Flow Charts for each `Ash.Flow`"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [only: :keep, format: :string],
        aliases: [o: :only, f: :format]
      )

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      end

    format = Keyword.get(opts, :format, "plain")

    flows()
    |> Task.async_stream(
      fn flow ->
        source = flow.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          Mix.Mermaid.generate_diagram(
            source,
            "mermaid-flowchart",
            format,
            Ash.Flow.Chart.Mermaid.chart(flow, expand?: false),
            "Generated Mermaid Flow Chart for #{inspect(flow)}"
          )

          if has_run_flow_step?(flow) do
            Mix.Mermaid.generate_diagram(
              source,
              "expanded-mermaid-flowchart",
              format,
              Ash.Flow.Chart.Mermaid.chart(flow, expand?: true),
              "Generated Expanded Mermaid Flow Chart for #{inspect(flow)}"
            )
          end
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp has_run_flow_step?(flow) do
    flow
    |> Ash.Flow.Info.steps()
    |> any_complex?()
  end

  defp any_complex?(steps) when is_list(steps) do
    Enum.any?(steps, &any_complex?/1)
  end

  defp any_complex?(%Ash.Flow.Step.RunFlow{}), do: true
  defp any_complex?(%{steps: steps}), do: any_complex?(steps)
  defp any_complex?(_), do: false

  defp modules do
    Mix.Project.config()[:app]
    |> :application.get_key(:modules)
    |> case do
      {:ok, mods} when is_list(mods) ->
        mods

      _ ->
        []
    end
  end

  defp flows do
    for module <- modules(),
        {:module, module} = Code.ensure_compiled(module),
        Spark.Dsl.is?(module, Ash.Flow) do
      module
    end
  end
end
