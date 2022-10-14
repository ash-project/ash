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

  """
  use Mix.Task

  @shortdoc "Generates Mermaid Flow Charts for each `Ash.Flow`"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} = OptionParser.parse!(argv, strict: [only: :keep], aliases: [o: :only])

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(List.wrap(opts[:only]), &Path.expand/1)
      end

    flows()
    |> Task.async_stream(
      fn flow ->
        source = flow.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          make_simple(flow, Mix.Mermaid.file(source, "mermaid-flowchart", "pdf"))
          make_expanded(flow, Mix.Mermaid.file(source, "expanded-mermaid-flowchart", "pdf"))

          Mix.shell().info("Generated Mermaid Flow Chart for #{inspect(flow)}")
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp make_simple(flow, file) do
    Mix.Mermaid.create_diagram(file, Ash.Flow.Chart.Mermaid.chart(flow, expand?: false))
  end

  defp make_expanded(flow, file) do
    if has_run_flow_step?(flow) do
      Mix.Mermaid.create_diagram(file, Ash.Flow.Chart.Mermaid.chart(flow, expand?: true))
    end
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

  def modules do
    Mix.Project.config()[:app]
    |> Application.get_env(:modules, [])
  end

  def flows do
    for module <- modules(),
        {:module, module} = Code.ensure_compiled(module),
        Spark.Dsl.is?(module, Ash.Flow) do
      module
    end
  end
end
