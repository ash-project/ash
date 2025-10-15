# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Validation do
    @moduledoc """
    Generates a custom validation

    See [Custom Validations](https://hexdocs.pm/ash/validations.html#custom-validations) for more.

    ## Example

    ```bash
    mix ash.gen.validation MyApp.Validations.IsPrime
    ```
    """
    @shortdoc "Generates a custom validation module."
    use Igniter.Mix.Task

    @impl true
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [:validation]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      validation = igniter.args.positional.validation
      validation = Igniter.Project.Module.parse(validation)

      igniter
      |> Igniter.Project.Module.create_module(validation, """
      use Ash.Resource.Validation

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def validate(_changeset, _opts, _context) do
        :ok
      end
      """)
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Validation do
    @moduledoc """
    Generates a custom validation

    See [Custom Validations](https://hexdocs.pm/ash/validations.html#custom-validations) for more.

    ## Example

    ```bash
    mix ash.gen.validation MyApp.Validations.IsPrime
    ```
    """
    @shortdoc "Generates a custom validation module."
    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.validation' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
