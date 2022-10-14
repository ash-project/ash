defmodule Mix.Mermaid do
  def config do
    if File.exists?("mermaidConfig.json") do
      "--configFile #{Path.expand("mermaidConfig.json")}"
    end
  end

  def file(source, suffix, extension) do
    filename =
      source
      |> Path.basename()
      |> Path.rootname()
      |> Kernel.<>("-" <> suffix <> "." <> extension)

    source
    |> Path.dirname()
    |> Path.join(filename)
  end

  def create_diagram(file, markdown) do
    "sh"
    |> System.cmd([
      "-c",
      """
      cat <<EOF | mmdc --output #{file} #{config()}
      #{markdown}
      EOF
      """
    ])
    |> case do
      {_, 0} ->
        :ok

      {text, exit_status} ->
        raise "Creating Mermaid diagram #{file} exited with status: #{exit_status}\n#{text}"
    end
  end
end
