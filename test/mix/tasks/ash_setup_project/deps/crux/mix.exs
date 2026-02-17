# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.MixProject do
  @moduledoc false

  use Mix.Project

  @description """
  Library for boolean satisfiability solving and expression manipulation.
  """

  @version "0.1.2"

  def project do
    [
      app: :crux,
      version: @version,
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps(),
      docs: &docs/0,
      description: @description,
      source_url: "https://github.com/ash-project/crux",
      homepage_url: "https://github.com/ash-project/crux"
    ]
  end

  def application do
    []
  end

  defp docs do
    [
      main: "Crux",
      source_ref: "v#{@version}",
      nest_modules_by_prefix: [Crux.Expression.RewriteRule],
      groups_for_modules: [
        "Rewrite Rules": [
          ~r/^Crux\.Expression\.RewriteRule\..+/
        ]
      ]
    ]
  end

  defp package do
    [
      maintainers: ["Ash Project"],
      licenses: ["MIT"],
      files: ~w(lib .formatter.exs mix.exs README* LICENSE*),
      links: %{
        "GitHub" => "https://github.com/ash-project/crux",
        "Changelog" => "https://github.com/ash-project/crux/releases",
        "Discord" => "https://discord.gg/HTHRaaVPUc",
        "Website" => "https://ash-hq.org",
        "Forum" => "https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum",
        "REUSE Compliance" => "https://api.reuse.software/info/github.com/ash-project/crux"
      }
    ]
  end

  defp deps do
    # styler:sort
    [
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:doctest_formatter, "~> 0.4.1", only: [:dev, :test], runtime: false},
      {:doctor, "~> 0.22.0", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.12", only: [:dev, :test]},
      {:ex_doc, "~> 0.37", only: [:dev, :test], runtime: false},
      {:git_ops, "~> 2.5", only: [:dev, :test]},
      {:mix_audit, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:mix_test_watch, "~> 1.0", only: [:dev, :test], runtime: false},
      {:picosat_elixir, "~> 0.2", optional: true},
      {:simple_sat, "~> 0.1 and >= 0.1.1", optional: true},
      {:sobelow, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:stream_data, "~> 1.0", optional: true},
      {:styler, "~> 1.9", only: [:dev, :test], runtime: false},
      {:usage_rules, "~> 0.1", only: [:dev]}
    ]
  end
end
