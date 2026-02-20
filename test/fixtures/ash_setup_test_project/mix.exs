# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule AshSetupTestProject.MixProject do
  use Mix.Project

  def project do
    [
      app: :ash_setup_test_project,
      version: "0.1.0",
      elixir: "~> 1.11",
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [{:ash, path: "../../../"}]
  end
end
