# SPDX-FileCopyrightText: 2019 ash contributors
# SPDX-License-Identifier: MIT

defmodule AshSetupProject.MixProject do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :ash_setup_fixture,
      version: "0.1.0",
      elixir: "~> 1.11",
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    # Path to ash repo root from test/mix/tasks/ash_setup_project
    [{:ash, path: "../../../.."}]
  end
end
