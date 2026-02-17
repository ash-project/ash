# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.GenerateIgniterErrorTest do
  @moduledoc """
  Generates test/ash/igniter_error_test.exs for Ash.Igniter error handling.
  Implementation lives in test/support/generate_igniter_error_test.ex.
  """
  use Mix.Task

  @shortdoc "Generates test file for igniter.ex error handling"
  @requirements ["app.config"]

  def run(args) do
    path = Path.join(File.cwd!(), "test/support/generate_igniter_error_test.ex")

    unless File.exists?(path) do
      Mix.shell().error("Error: generator not found at #{path}")
      exit({:shutdown, 1})
    end

    [{module, _}] = Code.compile_file(path)
    module.run(args)
  end
end
