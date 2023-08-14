defmodule Mix.Tasks.Ash.Setup do
  @moduledoc """
  Runs all setup tasks for any extension on any resource/api in your application.
  """
  use Mix.Task

  @shortdoc "Runs all setup tasks for any extension on any resource/api in your application."
  def run(argv) do
    argv
    |> Ash.Mix.Tasks.Helpers.extensions!()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :setup, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running setup for #{name}...")
        extension.setup(argv)
      end
    end)
  end
end
