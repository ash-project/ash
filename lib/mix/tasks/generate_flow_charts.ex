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
          directory = Path.dirname(source)

          make_simple(flow, source, directory)
          make_expanded(flow, source, directory)

          Mix.shell().info("Generated Mermaid Flow Chart for #{inspect(flow)}")
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp make_simple(flow, source, directory) do
    filename =
      source
      |> Path.basename()
      |> Path.rootname()
      |> Kernel.<>("-mermaid-flowchart.pdf")

    file = Path.join(directory, filename)

    create_flow_chart(file, Ash.Flow.Chart.Mermaid.chart(flow, expand?: false))
  end

  defp make_expanded(flow, source, directory) do
    if has_run_flow_step?(flow) do
      filename =
        source
        |> Path.basename()
        |> Path.rootname()
        |> Kernel.<>("-expanded-mermaid-flowchart.pdf")

      file = Path.join(directory, filename)

      create_flow_chart(file, Ash.Flow.Chart.Mermaid.chart(flow, expand?: true))
    end
  end

  defp create_flow_chart(file, text) do
    config =
      if File.exists?("mermaidConfig.json") do
        "--configFile #{Path.expand("mermaidConfig.json")}"
      end

    "sh"
    |> System.cmd([
      "-c",
      """
      cat <<EOF | mmdc --output #{file} #{config}
      #{text}
      EOF
      """
    ])
    |> case do
      {_, 0} ->
        :ok

      {text, exit_status} ->
        raise "Creating Mermaid Flow Chart #{file} exited with status: #{exit_status}\n#{text}"
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

  def flows do
    for module <- modules(),
        {:module, module} = Code.ensure_compiled(module),
        Spark.Dsl.is?(module, Ash.Flow) do
      module
    end
  end

  defp modules do
    app = Mix.Project.config()[:app]

    if app do
      {:ok, modules} = :application.get_key(app, :modules)
      modules
    else
      []
    end
  end
end
