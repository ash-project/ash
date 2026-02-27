# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Setup do
  @moduledoc """
  Runs all setup tasks for any extension on any resource/domain in your application.

  On failure, raises with an error message listing each failed task and its exception.
  Each failure is recorded as `{task_name, exception}` (e.g. `{"compile", error}` or
  `{extension_name, error}`).
  """
  use Mix.Task

  @shortdoc "Runs all setup tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  @spec run([String.t()], keyword()) :: :ok
  def run(argv, opts \\ []) do
    compile_fn = Keyword.get(opts, :compile, fn -> Mix.Task.run("compile") end)
    extensions_fn = Keyword.get(opts, :extensions, &Ash.Mix.Tasks.Helpers.extensions!/1)

    extensions = extensions_fn.(argv)

    failures = run_compile(compile_fn) ++ run_extensions(extensions, argv)

    if failures != [] do
      message =
        failures
        |> Enum.map(fn {task, error} ->
          "  • #{task}: #{Exception.message(error)}"
        end)
        |> Enum.join("\n")

      Mix.raise("""
      Setup failed with the following errors:

      #{message}
      """)
    else
      :ok
    end
  end

  defp run_compile(compile_fn) do
    try do
      compile_fn.()
      []
    rescue
      e -> [{"compile", e}]
    end
  end

  defp run_extensions(extensions, argv) do
    Enum.reduce(extensions, [], fn extension, acc ->
      if function_exported?(extension, :setup, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running setup for #{name}...")

        try do
          extension.setup(argv)
          acc
        rescue
          e -> acc ++ [{name, e}]
        end
      else
        acc
      end
    end)
  end
end
