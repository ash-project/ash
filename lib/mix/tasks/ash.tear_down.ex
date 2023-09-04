defmodule Mix.Tasks.Ash.TearDown do
  @moduledoc """
  Runs all tear down tasks for any extension on any resource/api in your application.
  """
  use Mix.Task

  @shortdoc "Runs all tear_down tasks for any extension on any resource/api in your application."
  def run(argv) do
    argv
    |> Ash.Mix.Tasks.Helpers.extensions!()
    |> Enum.map(fn extension ->
      if function_exported?(extension, :tear_down, 1) do
        name =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end

        Mix.shell().info("Running tear_down for #{name}...")
        extension.tear_down(argv)
      end
    end)
  end
end
