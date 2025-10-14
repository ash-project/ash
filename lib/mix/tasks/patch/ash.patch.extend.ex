# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Patch.Extend do
    @moduledoc """
    Adds an extension or extensions to the domain/resource. Use `ash.extend` instead.
    """
    @shortdoc "Adds an extension or extensions to the given domain/resource"
    require Igniter.Code.Common
    use Igniter.Mix.Task

    @impl Igniter.Mix.Task
    def info(argv, parent) do
      Mix.Tasks.Ash.Extend.info(argv, parent)
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      Mix.Tasks.Ash.Extend.igniter(igniter)
    end
  end
else
  defmodule Mix.Tasks.Ash.Patch.Extend do
    @moduledoc """
    Adds an extension or extensions to the domain/resource Use `ash.extend` instead.
    """
    @shortdoc "Adds an extension or extensions to the given domain/resource"

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.patch.extend' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
