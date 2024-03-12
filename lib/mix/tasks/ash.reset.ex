defmodule Mix.Tasks.Ash.Reset do
  @moduledoc """
  Runs all tear down tasks for any extension on any resource/domain in your application, followed by setup tasks.
  """
  use Mix.Task

  @shortdoc "Runs all tear down & setup tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    Mix.Task.run("compile")
    Mix.Task.run("ash.tear_down", argv)
    Mix.Task.run("ash.setup", argv)
  end
end
