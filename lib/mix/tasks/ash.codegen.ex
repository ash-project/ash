# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Codegen do
  @moduledoc """
  Runs all codegen tasks for any extension on any resource/domain in your application.

  ## Flags

  * `--dry-run` - no files are created, instead the new generated code is printed to the console
  * `--check` - no files are created, returns an exit(1) code if any code would need to be generated
  * `--dev` - runs codegen tasks in dev mode. See the section on dev mode below
  * Individual extensions may use additional flags.

  ## Dev Mode

  Some extensions that do `codegen` require providing a `name`. Those extensions (should) support a `--dev`
  flag, which indicates that the codegen can have a temporary name chosen by the extension. Then, once you
  are ready to commit the changes, you can run the codegen tasks again without the `--dev` flag and with a
  name to generate the final code.

  For example, using AshPostgres:

  - First we add `first_name` to `MyApp.Accounts.User`
  - `mix ash.codegen --dev`, which generates a migration for adding `first_name` to the `"users"` table,
    but the migration is suffixed with `_dev`
  - `mix ash.migrate` apply the migrations
  - Then we add `last_name` to `MyApp.Accounts.User`
  - `mix ash.codegen --dev` which generates a migration for adding `last_name` to the `"users"` table,
    but the migration is suffixed with `_dev`
  - We review our changes, and are ready to save them as a unit
  - `mix ash.codegen add_name_to_user`, which rolls back and deletes the dev migrations & snapshots, and creates new
    ones using the provided name
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

        argv =
          if "--name" in argv do
            argv
          else
            argv ++ ["--name", name]
          end

        extension.codegen(argv)
      end
    end)
  rescue
    e in Ash.Error.Framework.PendingCodegen ->
      Mix.raise(Exception.message(e) <> "\n\nRun with --dry-run to view pending changes")
  end
end
