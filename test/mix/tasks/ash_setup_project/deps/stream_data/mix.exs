defmodule StreamData.Mixfile do
  use Mix.Project

  @version "1.2.0"
  @repo_url "https://github.com/whatyouhide/stream_data"

  def project() do
    [
      app: :stream_data,
      version: @version,
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Tests
      test_coverage: [tool: ExCoveralls],

      # Docs
      name: "StreamData",
      docs: [
        source_ref: "v#{@version}",
        main: "StreamData",
        source_url: @repo_url
      ],

      # Hex
      description: "Data generation and property-based testing for Elixir",
      package: [
        maintainers: ["Andrea Leopardi"],
        licenses: ["Apache-2.0"],
        links: %{"GitHub" => @repo_url, "Sponsor" => "https://github.com/sponsors/whatyouhide"}
      ],

      # Dialyxir
      dialyzer: [
        plt_add_deps: :apps_direct,
        plt_file: {:no_warn, "priv/plts/project.plt"},
        plt_add_apps: [:ex_unit]
      ]
    ]
  end

  def application() do
    [
      extra_applications: [],
      env: [
        initial_size: 1,
        max_runs: 100,
        max_run_time: :infinity,
        max_shrinking_steps: 100
      ]
    ]
  end

  defp deps() do
    [
      # Dev/test dependencies.
      {:dialyxir, "~> 1.3", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.29", only: :dev},
      {:excoveralls, "~> 0.18.0", only: :test}
    ]
  end
end
