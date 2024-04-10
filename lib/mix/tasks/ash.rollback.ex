defmodule Mix.Tasks.Ash.Rollback do
  @moduledoc """
  Runs all rollback tasks for any extension on any resource/domain in your application.
  """
  use Mix.Task

  @shortdoc "Runs all rollback tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

    argv
    |> Ash.Mix.Tasks.Helpers.extensions!()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :rollback, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running rollback for #{name}...")

        extension.rollback(argv)
      end
    end)
  end
end
