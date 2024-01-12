defmodule Mix.Mermaid do
  @moduledoc false

  @doc """
  Generate the option string for a mermaid config file if it exists.
  """
  def config do
    if File.exists?("mermaidConfig.json") do
      "--configFile #{Path.expand("mermaidConfig.json")}"
    end
  end

  # sobelow_skip ["Traversal"]
  def generate_diagram(source, suffix, "plain", markdown, message) do
    file = Mix.Mermaid.file(source, suffix, "mmd")

    File.write!(file, markdown)

    Mix.shell().info(message <> " (#{file})")
  end

  # sobelow_skip ["Traversal"]
  def generate_diagram(source, suffix, "md", markdown, message) do
    file = Mix.Mermaid.file(source, suffix, "md")

    File.write!(file, """
    ```mermaid
    #{markdown}
    ```
    """)

    Mix.shell().info(message <> " (#{file})")
  end

  def generate_diagram(source, suffix, format, markdown, message)
      when format in ["svg", "pdf", "png"] do
    file = Mix.Mermaid.file(source, suffix, format)

    Mix.Mermaid.create_diagram(file, markdown)

    Mix.shell().info(message <> " (#{file})")
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
      cat <<EOF | mmdc --output #{file} --pdfFit #{config()}
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
