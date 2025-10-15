# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Gen.Change do
    @moduledoc """
    Generates a custom change

    See [Custom Changes](https://hexdocs.pm/ash/changes.html#custom-changes) for more.

    ## Example

    ```bash
    mix ash.gen.change MyApp.Changes.Slugify
    ```
    """
    @shortdoc "Generates a custom change module."
    use Igniter.Mix.Task

    @impl true
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        positional: [:change]
      }
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      change = igniter.args.positional.change
      change = Igniter.Project.Module.parse(change)

      igniter
      |> Igniter.Project.Module.create_module(change, """
      use Ash.Resource.Change

      @impl true
      def init(opts) do
        {:ok, opts}
      end

      @impl true
      def change(changeset, _opts, _context) do
        changeset
      end
      """)
    end
  end
else
  defmodule Mix.Tasks.Ash.Gen.Change do
    @moduledoc """
    Generates a custom change

    See [Custom Changes](https://hexdocs.pm/ash/changes.html#custom-changes) for more.

    ## Example

    ```bash
    mix ash.gen.change MyApp.Changes.Slugify
    ```
    """
    @shortdoc "Generates a custom change module."
    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.gen.change' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
