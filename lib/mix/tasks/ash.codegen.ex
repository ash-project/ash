defmodule Mix.Tasks.Ash.Codegen do
  @moduledoc """
  Runs all codegen tasks for any extension on any resource/api in your application.
  """
  use Mix.Task

  @shortdoc "Runs all codegen tasks for any extension on any resource/api in your application."
  def run(argv) do
    {name, argv} =
      case argv do
        ["-" <> _ | _] ->
          {nil, argv}

        [first | rest] ->
          {first, rest}

        [] ->
          {nil, []}
      end

    {opts, _} =
      OptionParser.parse!(argv,
        strict: [
          name: :string,
          no_format: :boolean,
          dry_run: :boolean,
          check: :boolean,
          drop_columns: :boolean
        ]
      )

    if !opts[:name] && !opts[:dry_run] && !opts[:check] do
      raise ArgumentError, """
      Name must be provided when running `ash.codegen`, unless `--dry-run` or `--check` is also provided.

      Please provide a name. for example:

          mix ash.codegen add_feature_for_reticulating_splines #{Enum.join(argv, " ")}
      """
    end

    argv
    |> Ash.Mix.Tasks.Helpers.extensions!()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :codegen, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running codegen for #{name}...")

        extension.codegen(argv)
      end
    end)
  end
end
