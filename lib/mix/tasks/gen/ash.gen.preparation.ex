# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Preparation do
    @moduledoc """
    Generates a custom preparation

    See [Custom Preparations](https://hexdocs.pm/ash/preparations.html#custom-preparations) for more.

    ## Example

    ```bash
    mix ash.gen.preparation MyApp.Preparations.Top5
    ```
    """
    @shortdoc "Generates a custom preparation module."
    use Igniter.Mix.Task

    @impl true
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [:preparation]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      preparation = igniter.args.positional.preparation
      preparation = Igniter.Project.Module.parse(preparation)

      igniter
      |> Igniter.Project.Module.create_module(preparation, """
      use Ash.Resource.Preparation

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def prepare(query, _opts, _context) do
        query
      end
      """)
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Preparation do
    @moduledoc """
    Generates a custom preparation

    See [Custom Preparations](https://hexdocs.pm/ash/preparations.html#custom-preparations) for more.

    ## Example

    ```bash
    mix ash.gen.preparation MyApp.Preparations.Top5
    ```
    """
    @shortdoc "Generates a custom preparation module."
    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.preparation' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
