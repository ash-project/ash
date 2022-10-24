defmodule Mix.Mermaid do
  @moduledoc """
  Mermaid Diagram helper functions.
  """

  @doc """
  Generate the option string for a mermaid config file if it exists.
  """
  def config do
    if File.exists?("mermaidConfig.json") do
      "--configFile #{Path.expand("mermaidConfig.json")}"
    end
  end

  def generate_diagram(source, suffix, "plain", markdown, message) do
    source
    |> Mix.Mermaid.file(suffix, "mermaid")
    |> File.write!(markdown)

    Mix.shell().info(message)
  end

  def generate_diagram(source, suffix, "md", markdown, message) do
    source
    |> Mix.Mermaid.file(suffix, "md")
    |> File.write!("""
    ```mermaid
    #{markdown}
    ```
    """)

    Mix.shell().info(message)
  end

  def generate_diagram(source, suffix, format, markdown, message)
      when format in ["svg", "pdf", "png"] do
    source
    |> Mix.Mermaid.file(suffix, format)
    |> Mix.Mermaid.create_diagram(markdown)

    Mix.shell().info(message)
  end

  def generate_diagram(_, _, format, _, _) do
    Mix.shell().error("""
    Invalid format `#{format}`.

    Valid options are `plain`, `md`, `svg`, `pdf` or `png`.
    """)
  end

  @doc """
  Generate a diagram filename next to the source file.
  """
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

  @doc """
  Generate a Mermaid diagram using the CLI.

  For more info see https://github.com/mermaid-js/mermaid-cli
  """
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
