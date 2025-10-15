# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.CustomExpression do
    @moduledoc """
    Generates a custom expression

    See [Ash.CustomExpression](https://hexdocs.pm/ash/Ash.CustomExpression.html) for more.

    ## Example

    ```bash
    mix ash.gen.custom_expression MyApp.Expressions.LevenshteinDistance --args string,string
    ```

    ## Options

    * `--args` - A comma separated list of argument types. i.e `--args string`
    """
    @shortdoc "Generates a custom expression module."
    use Igniter.Mix.Task

    @impl true
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [:expression],
        schema: [args: :csv, name: :string]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      expression = igniter.args.positional.expression
      options = igniter.args.options
      name = fetch_name(expression, options)
      args = fetch_args(options)
      expression = Igniter.Project.Module.parse(expression)

      igniter
      |> Igniter.Project.Module.create_module(expression, """
      use Ash.CustomExpression,
        name: :#{name},
        args: [#{args}]

      @impl true
      def expression(_data_layer, args) do
        {:ok, expr(args)}
      end
      """)
    end

    defp fetch_name(expression, options) do
      case options[:name] do
        nil ->
          expression
          |> String.split(".")
          |> List.last()
          |> Macro.underscore()

        name ->
          name
      end
    end

    defp fetch_args(options) do
      case options[:args] do
        nil -> ""
        args -> Enum.map_join(args, ",", &":#{&1}")
      end
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.CustomExpression do
    @moduledoc """
    Generates a custom expression

    See [Ash.CustomExpression](https://hexdocs.pm/ash/Ash.CustomExpression.html) for more.

    ## Example

    ```bash
    mix ash.gen.expression MyApp.Expressions.LevenshteinDistance --args string,string
    ```
    """
    @shortdoc "Generates a custom expression module."
    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.expression' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
