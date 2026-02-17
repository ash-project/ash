# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Reactor.Mermaid do
  @shortdoc "Generate Mermaid diagrams from Reactor modules"
  @moduledoc """
  #{@shortdoc}

  The `reactor.mermaid` mix task generates Mermaid flowchart diagrams from reactor modules,
  making it easy to visualize workflow structure and dependencies.

  ## Examples

      # Generate diagram for a single reactor
      $ mix reactor.mermaid MyApp.UserRegistrationReactor

      # Save to a specific file
      $ mix reactor.mermaid MyApp.UserRegistrationReactor --output user_flow.mmd

      # Include descriptions and expand sub-reactors
      $ mix reactor.mermaid MyApp.PaymentReactor --describe --expand

      # Generate from multiple reactors
      $ mix reactor.mermaid MyApp.Reactor1 MyApp.Reactor2 --output combined.mmd

  ## Options

    * `--output` (`-o`) - Output file path. If not specified, saves as `<reactor_name>.mmd`
    * `--describe` (`-d`) - Include step descriptions in the diagram
    * `--expand` (`-e`) - Expand composed sub-reactors inline
    * `--direction` - Diagram direction: `top_to_bottom` (default), `left_to_right`, `bottom_to_top`, `right_to_left`
    * `--format` (`-f`) - Output format: `mermaid` (default), `copy`, `url`

  ## Output Formats

    * `mermaid` - Saves the Mermaid diagram source code to a file
    * `copy` - Displays the diagram source for copy-pasting into Mermaid Live Editor
    * `url` - Generates a direct Mermaid Live Editor URL

  ## Usage Tips

  View generated diagrams at:
    * https://mermaid.live/edit (paste diagram source)
    * VS Code with Mermaid extension
    * GitHub (native Mermaid support in markdown)
  """
  use Mix.Task

  @switches [
    output: :string,
    describe: :boolean,
    expand: :boolean,
    direction: :string,
    format: :string
  ]

  @aliases [
    o: :output,
    d: :describe,
    e: :expand,
    f: :format
  ]

  @directions ~w(top_to_bottom left_to_right bottom_to_top right_to_left)
  @formats ~w(mermaid copy url)

  @doc false
  @impl true
  def run([]), do: Mix.Task.run("help", ["reactor.mermaid"])

  def run(args) do
    {opts, reactors, _} = OptionParser.parse(args, switches: @switches, aliases: @aliases)

    with :ok <- validate_options(opts),
         {:ok, reactor_modules} <- validate_reactors(reactors),
         {:ok, options} <- build_mermaid_options(opts),
         {:ok, output_file} <- determine_output_file(opts, reactor_modules),
         {:ok, diagram} <- generate_diagram(reactor_modules, options),
         {:ok, result} <- save_output(diagram, output_file, opts[:format] || "mermaid") do
      case opts[:format] do
        "copy" ->
          Mix.shell().info("✅ Mermaid diagram for copy-paste:")
          Mix.shell().info(result)

        "url" ->
          Mix.shell().info("✅ Mermaid Live URL generated:")
          Mix.shell().info("🌐 #{result}")

        _ ->
          Mix.shell().info("✅ Mermaid diagram generated: #{result}")
          Mix.shell().info("📊 View at: https://mermaid.live/edit")
      end
    else
      {:error, message} ->
        Mix.shell().error("❌ #{message}")
        System.stop(1)
    end
  end

  defp validate_options(opts) do
    with :ok <- validate_direction(opts[:direction]) do
      validate_format(opts[:format])
    end
  end

  defp validate_direction(nil), do: :ok
  defp validate_direction(direction) when direction in @directions, do: :ok

  defp validate_direction(direction) do
    {:error, "Invalid direction '#{direction}'. Must be one of: #{Enum.join(@directions, ", ")}"}
  end

  defp validate_format(nil), do: :ok
  defp validate_format(format) when format in @formats, do: :ok

  defp validate_format(format) do
    {:error, "Invalid format '#{format}'. Must be one of: #{Enum.join(@formats, ", ")}"}
  end

  defp validate_reactors([]) do
    {:error, "Please specify at least one reactor module"}
  end

  defp validate_reactors(reactors) do
    reactors
    |> Enum.reduce_while({:ok, []}, fn reactor_name, {:ok, acc} ->
      case validate_reactor(reactor_name) do
        {:ok, reactor} -> {:cont, {:ok, [reactor | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, reactors} -> {:ok, Enum.reverse(reactors)}
      error -> error
    end
  end

  defp validate_reactor(reactor_name) do
    with {:ok, reactor} <- try_load_module(reactor_name),
         :ok <- reactor?(reactor) do
      {:ok, reactor}
    end
  end

  defp try_load_module(module) do
    module = Module.concat([String.trim(module)])

    case Code.ensure_loaded(module) do
      {:module, module} ->
        {:ok, module}

      {:error, reason} ->
        {:error, "Unable to load reactor '#{inspect(module)}': #{inspect(reason)}"}
    end
  end

  defp reactor?(module) do
    if function_exported?(module, :spark_is, 0) && module.spark_is() == Reactor do
      :ok
    else
      {:error, "Module '#{inspect(module)}' is not a Reactor module"}
    end
  end

  defp build_mermaid_options(opts) do
    options = [
      describe?: opts[:describe] || false,
      expand?: opts[:expand] || false
    ]

    options =
      case opts[:direction] do
        nil -> options
        direction -> Keyword.put(options, :direction, String.to_atom(direction))
      end

    {:ok, options}
  end

  defp determine_output_file(opts, reactors) do
    case opts[:output] do
      nil when length(reactors) == 1 ->
        [reactor] = reactors
        name = reactor |> Module.split() |> List.last() |> Macro.underscore()
        {:ok, "#{name}.mmd"}

      nil ->
        {:ok, "reactors.mmd"}

      output_file ->
        {:ok, output_file}
    end
  end

  defp generate_diagram([reactor], options) do
    Reactor.Mermaid.to_mermaid(reactor, options)
  end

  defp generate_diagram(reactors, options) when length(reactors) > 1 do
    diagrams =
      Enum.map(reactors, fn reactor ->
        case Reactor.Mermaid.to_mermaid(reactor, options) do
          {:ok, diagram} ->
            name = reactor |> Module.split() |> List.last()
            "subgraph #{name}\n#{indent_diagram(diagram)}\nend"

          {:error, _} = error ->
            throw(error)
        end
      end)

    diagram = """
    flowchart TB
    #{Enum.join(diagrams, "\n\n")}
    """

    {:ok, diagram}
  catch
    {:error, _} = error -> error
  end

  defp indent_diagram(diagram) do
    diagram
    |> String.split("\n")
    # Remove the "flowchart TB" line
    |> Enum.drop(1)
    |> Enum.map_join("\n", &("  " <> &1))
  end

  defp save_output(diagram, output_file, "mermaid") do
    case File.write(output_file, to_string(diagram)) do
      :ok -> {:ok, output_file}
      error -> error
    end
  end

  defp save_output(diagram, _output_file, "copy") do
    # For copy format, return instructions to copy-paste
    diagram_string = to_string(diagram)

    message = """
    Copy the following Mermaid diagram and paste it at https://mermaid.live/edit:

    #{diagram_string}
    """

    {:ok, message}
  end

  defp save_output(diagram, _output_file, "url") do
    # Generate Mermaid Live Editor URL with proper JSON state
    diagram_string = to_string(diagram)

    state = %{
      "code" => diagram_string,
      "mermaid" => %{
        "theme" => "default"
      },
      "autoSync" => true,
      "rough" => false
    }

    json_state = Jason.encode!(state)
    encoded = json_state |> Base.encode64()
    url = "https://mermaid.live/edit#base64:#{encoded}"
    {:ok, url}
  end
end
