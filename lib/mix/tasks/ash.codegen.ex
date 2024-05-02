defmodule Mix.Tasks.Ash.Codegen do
  @moduledoc """
  Runs all codegen tasks for any extension on any resource/domain in your application.
  """
  use Mix.Task

  @shortdoc "Runs all codegen tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

    {name, argv} =
      case argv do
        ["-" <> _ | _] ->
          {nil, argv}

        [first | rest] ->
          {String.trim(first), rest}

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

    opts = Keyword.put_new(opts, :name, name)

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
        extension_name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running codegen for #{extension_name}...")

        extension.codegen(argv ++ ["--name", name])
      end
    end)
  end
end
