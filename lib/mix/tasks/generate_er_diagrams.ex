defmodule Mix.Tasks.Ash.GenerateErDiagrams do
  @moduledoc """
  Generates Mermaid ER Diagram PNGs for each Ash API.
  """
  use Mix.Task

  @shortdoc "Generates Mermaid ER Diagrams for each Ash API"
  def run(argv) do
    Mix.Task.run("compile")

    {opts, _} = OptionParser.parse!(argv, strict: [only: :keep], aliases: [o: :only])

    only =
      if opts[:only] && opts[:only] != [] do
        Enum.map(opts[:only], &Path.expand/1)
      end

    apis()
    |> Task.async_stream(
      fn api ->
        source = api.module_info(:compile)[:source]

        if is_nil(only) || Path.expand(source) in only do
          directory = Path.dirname(source)

          make_diagram(api, source, directory)

          Mix.shell().info("Generated ER diagram for #{inspect(api)}")
        end
      end,
      timeout: :infinity
    )
    |> Stream.run()
  end

  defp make_diagram(api, source, directory) do
    filename =
      source
      |> Path.basename()
      |> Path.rootname()
      |> Kernel.<>("-mermaid-chart.png")

    file = Path.join(directory, filename)

    create_er_diagram(file, Ash.Api.Info.Diagram.mermaid_er_diagram(api))
  end

  defp create_er_diagram(file, text) do
    config =
      if File.exists?("mermaidConfig.json") do
        "--configFile #{Path.expand("mermaidConfig.json")}"
      end

    "sh"
    |> System.cmd([
      "-c",
      """
      cat <<EOF | mmdc --output #{file} #{config} --scale 10
      #{text}
      EOF
      """
    ])
    |> case do
      {_, 0} ->
        :ok

      {text, exit_status} ->
        raise "Creating Mermaid ER Diagram #{file} exited with status: #{exit_status}\n#{text}"
    end
  end

  def sibling_file(file) do
    __ENV__.file
    |> Path.dirname()
    |> Path.join(file)
  end

  def apis do
    Application.get_application(__MODULE__) |> IO.inspect()

    for module <- modules(),
        {:module, module} = Code.ensure_compiled(module),
        Spark.Dsl.is?(module, Ash.Api) do
      module
    end
    |> dbg
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
