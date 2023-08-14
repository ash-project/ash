defmodule Mix.Tasks.Ash.Migrate do
  @moduledoc """
  Runs all migration tasks for any extension on any resource/api in your application.
  """
  use Mix.Task

  @shortdoc "Runs all migration tasks for any extension on any resource/api in your application."
  def run(argv) do
    argv
    |> Ash.Mix.Tasks.Helpers.extensions!()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :migrate, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running migration for #{name}...")
        extension.migrate(argv)
      end
    end)
  end
end
