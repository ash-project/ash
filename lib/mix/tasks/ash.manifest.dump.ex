# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Manifest.Dump do
  @shortdoc "Dump the Ash app manifest as JSON"
  @moduledoc """
  Generates a JSON manifest of the application's public Ash surface
  (resources, types, and action entrypoints).

      mix ash.manifest.dump [--output FILE] [--format json]

  ## Options

    * `--output` / `-o` - Output file path (default: stdout)
    * `--format` - Output format, currently only "json" (default: "json")

  ## Examples

      mix ash.manifest.dump
      mix ash.manifest.dump --output manifest.json
      mix ash.manifest.dump -o manifest.json --format json
  """

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    Mix.Task.run("compile")

    {opts, _remaining, _invalid} =
      OptionParser.parse(args,
        switches: [output: :string, format: :string],
        aliases: [o: :output]
      )

    otp_app = Mix.Project.config()[:app]
    format = Keyword.get(opts, :format, "json")
    output = Keyword.get(opts, :output)

    case format do
      "json" ->
        generate_json(otp_app, output)

      other ->
        Mix.raise("Unsupported format: #{other}. Currently only \"json\" is supported.")
    end
  end

  defp generate_json(otp_app, output) do
    {:ok, spec} = Ash.Info.Manifest.generate(otp_app: otp_app)

    case Ash.Info.Manifest.JsonSerializer.to_json(spec, pretty: true) do
      {:ok, json} ->
        if output do
          File.write!(output, json)
          Mix.shell().info("Manifest written to #{output}")
        else
          Mix.shell().info(json)
        end

      {:error, error} ->
        Mix.raise("Failed to serialize manifest: #{error}")
    end
  end
end
