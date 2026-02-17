defmodule Ymlr.MixProject do
  use Mix.Project

  @source_url "https://github.com/ufirstgroup/ymlr"
  @version "5.1.4"

  def project do
    [
      app: :ymlr,
      description: "A YAML encoder for Elixir",
      version: @version,
      elixir: "~> 1.12",
      deps: deps(),
      dialyzer: dialyzer(),
      package: package(),
      preferred_cli_env: cli_env(),
      consolidate_protocols: Mix.env() != :test,
      test_coverage: [
        tool: ExCoveralls
      ],
      docs: [
        main: "readme",
        extras: ["README.md", "usage.livemd", "CHANGELOG.md", "BENCHMARK.md"],
        source_ref: "v#{@version}",
        source_url: @source_url
      ],
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp cli_env do
    [
      coveralls: :test,
      "coveralls.detail": :test,
      "coveralls.post": :test,
      "coveralls.html": :test,
      "coveralls.travis": :test,
      "coveralls.github": :test,
      "coveralls.xml": :test,
      "coveralls.json": :test,
      "test.watch": :test
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.5-pre", only: [:dev, :test], runtime: false},
      {:decimal, "~> 2.0", only: [:test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.18", only: [:test]},
      {:ex_doc, "~> 0.38", only: :dev},
      {:mix_test_watch, "~> 1.0", only: :test, runtime: false},
      {:yaml_elixir, "~> 2.4", only: [:test]}
    ]
  end

  defp dialyzer do
    [
      ignore_warnings: ".dialyzer_ignore.exs",
      flags: [:no_opaque],
      plt_add_apps: [:mix, :eex],
      plt_core_path: "priv/plts",
      plt_local_path: "priv/plts"
    ]
  end

  defp package do
    [
      name: :ymlr,
      maintainers: ["Michael Ruoss", "Jean-Luc Geering"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url
      },
      files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG.md"]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test_support"]
  defp elixirc_paths(_), do: ["lib"]
end
