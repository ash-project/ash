# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Enum do
    @example "mix ash.gen.enum MyApp.Support.Ticket.Types.Status open,closed --short-name ticket_status"
    @moduledoc """
    Generates an Ash.Type.Enum

    ## Example

    ```bash
    #{@example}
    ```

    ## Options

    - `--short-name`, `-s`: Register the type under the provided shortname, so it can be referenced like `:short_name` instead of the module name.
    - `--ignore-if-exists` - Does nothing if the resource already exists
    """

    @shortdoc "Generates an Ash.Type.Enum"
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        schema: [
          short_name: :string,
          ignore_if_exists: :boolean
        ],
        example: @example,
        positional: [:module_name, :types],
        aliases: [
          s: :short_name
        ]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      module_name = igniter.args.positional.module_name
      types = igniter.args.positional.types
      opts = igniter.args.options

      enum = Igniter.Project.Module.parse(module_name)
      file_name = Igniter.Project.Module.proper_location(igniter, enum)

      {exists?, igniter} = Igniter.Project.Module.module_exists(igniter, enum)

      if "--ignore-if-exists" in igniter.args.argv_flags && exists? do
        igniter
      else
        short_name =
          if opts[:short_name] do
            String.to_atom(opts[:short_name])
          end

        types =
          types
          |> String.split(",")
          |> Enum.map(&String.to_atom/1)

        igniter
        |> Igniter.create_new_file(file_name, """
        defmodule #{inspect(enum)} do
          use Ash.Type.Enum, values: #{inspect(types)}
        end
        """)
        |> then(fn igniter ->
          if short_name do
            Igniter.Project.Config.configure(
              igniter,
              "config.exs",
              :ash,
              [:custom_types, short_name],
              enum
            )
          else
            igniter
          end
        end)
      end
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Enum do
    @example "mix ash.gen.enum MyApp.Support.Ticket.Types.Status open,closed --short-name ticket_status"
    @moduledoc """
    Generates an Ash.Type.Enum

    ## Example

    ```bash
    #{@example}
    ```

    ## Options

    - `--short-name`, `-s`: Register the type under the provided shortname, so it can be referenced like `:short_name` instead of the module name.
    """

    @shortdoc "Generates an Ash.Type.Enum"

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.enum' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
