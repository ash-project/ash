defmodule Mix.Tasks.Ash.TearDown do
  @moduledoc """
  Runs all tear down tasks for any extension on any resource/domain in your application.
  """
  use Mix.Task

  @shortdoc "Runs all tear_down tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")

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
