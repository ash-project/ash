defmodule Graph.Mixfile do
  use Mix.Project

  def project do
    [
      app: :libgraph,
      version: "0.16.0",
      elixir: "~> 1.10",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      description: "A high-performance graph datastructure library for Elixir projects",
      package: package(),
      test_coverage: [tool: ExCoveralls],
      aliases: aliases(),
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.json": :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        docs: :docs,
        bench: :bench
      ]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: []]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:benchee, "~> 1.0", only: [:bench]},
      {:stream_data, "~> 0.5", only: [:test]},
      {:excoveralls, "~> 0.7", only: [:test]},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp aliases do
    [
      bench: [
        "bench.cliques",
        "bench.create",
        "bench.k_core",
        "bench.shortest_path",
        "bench.topsort"
      ],
      "bench.cliques": ["run bench/cliques.exs"],
      "bench.create": ["run bench/create.exs"],
      "bench.k_core": ["run bench/k_core.exs"],
      "bench.shortest_path": ["run bench/shortest_path.exs"],
      "bench.topsort": ["run bench/topsort.exs"]
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Paul Schoenfelder"],
      licenses: ["MIT"],
      links: %{GitHub: "https://github.com/bitwalker/libgraph"}
    ]
  end
end
