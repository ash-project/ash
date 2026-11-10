# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Signals do
  @moduledoc """
  Prints the signals of your application: the actions that emit each signal, and the listeners
  that react to it.

      $ mix ash.signals
      $ mix ash.signals MyApp.Shop.Signals
      $ mix ash.signals --format mermaid --output signals.mmd

  Pass signal modules to only print their signals.

  Listeners are found wherever they are defined. Emitters are found in the resources of the
  domains in your `:ash_domains` config, and in resources that listen to signals.

  ## Command line options

    * `--format` - Can be set to one of either:
      * `text` - A list of each signal's emitters and listeners. This is the default.
      * `mermaid` - A Mermaid flowchart.
      * `md` - A Mermaid flowchart in a markdown code block.
      * `svg`, `pdf` or `png` - An image of the Mermaid flowchart. Requires `--output`, and the
        Mermaid CLI. See https://github.com/mermaid-js/mermaid-cli
    * `--output` - Writes to the given file, instead of printing.
  """
  use Mix.Task

  @recursive true

  @shortdoc "Prints the signals of your application, and what emits and listens to them"
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

    {opts, signal_modules} =
      OptionParser.parse!(argv,
        strict: [format: :string, output: :string],
        aliases: [f: :format, o: :output]
      )

    entries =
      Ash.Signals.Graph.build(
        resources: resources(),
        signal_modules: if(signal_modules != [], do: Enum.map(signal_modules, &signal_module!/1))
      )

    case {Keyword.get(opts, :format, "text"), opts[:output]} do
      {"text", nil} when entries == [] ->
        Mix.shell().info("No signals found.")

      {"text", output} ->
        write(Ash.Signals.Graph.text(entries), output)

      {"mermaid", output} ->
        write(Ash.Signals.Graph.mermaid(entries), output)

      {"md", output} ->
        write("```mermaid\n#{Ash.Signals.Graph.mermaid(entries)}```\n", output)

      {format, nil} when format in ["svg", "pdf", "png"] ->
        Mix.raise("The `#{format}` format requires the `--output` option.")

      {format, output} when format in ["svg", "pdf", "png"] ->
        Mix.Mermaid.create_diagram(output, Ash.Signals.Graph.mermaid(entries))
        Mix.shell().info("Generated signal map (#{output})")

      {format, _} ->
        Mix.raise("""
        Invalid format `#{format}`.

        Valid options are `text`, `mermaid`, `md`, `svg`, `pdf` or `png`.
        """)
    end
  end

  defp write(content, nil), do: Mix.shell().info(String.trim_trailing(content))

  # sobelow_skip ["Traversal"]
  defp write(content, output) do
    File.write!(output, content)
    Mix.shell().info("Generated signal map (#{output})")
  end

  defp signal_module!(name) do
    module = Module.concat([name])

    if Code.ensure_loaded?(module) && Spark.Dsl.is?(module, Ash.Signals) do
      module
    else
      Mix.raise("`#{name}` is not an `Ash.Signals` module.")
    end
  end

  defp resources do
    Mix.Project.config()[:app]
    |> Application.get_env(:ash_domains, [])
    |> Enum.flat_map(&Ash.Domain.Info.resources/1)
  end
end
