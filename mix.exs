defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "1.0.0"

  def project do
    [
      app: :ash,
      version: @version,
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      deps: deps(),
      dialyzer: [plt_add_apps: [:mix, :mnesia]],
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.github": :test
      ],
      docs: docs(),
      aliases: aliases(),
      description: @description,
      source_url: "https://github.com/ash-project/ash",
      homepage_url: "https://github.com/ash-project/ash"
    ]
  end

  defp docs do
    # The main page in the docs
    [
      main: "Ash",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      groups_for_modules: [
        entrypoint: [
          Ash,
          Ash.Api,
          Ash.Resource,
          Ash.Dsl,
          Ash.Query
        ],
        type: ~r/Ash.Type/,
        data_layer: ~r/Ash.DataLayer/,
        authorizer: ~r/Ash.Authorizer/,
        extension: [
          Ash.Dsl.Entity,
          Ash.Dsl.Extension,
          Ash.Dsl.Section
        ],
        "resource dsl transformers": ~r/Ash.Resource.Transformers/,
        "resource dsl": ~r/Ash.Dsl/,
        "api dsl transformers": ~r/Ash.Api.Transformers/,
        "api dsl": ~r/Ash.Api.Dsl/,
        "filter predicates": ~r/Ash.Filter.Predicate/,
        filter: ~r/Ash.Filter/
      ]
    ]
  end

  defp package do
    [
      name: :ash,
      licenses: ["MIT"],
      links: %{
        GitHub: "https://github.com/ash-project/ash"
      }
    ]
  end

  defp elixirc_paths(:test) do
    ["lib", "test/support"]
  end

  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ecto, "~> 3.4"},
      {:ets, "~> 0.8.0"},
      {:ex_doc, "~> 0.22", only: :dev, runtime: false},
      {:ex_check, "~> 0.11.0", only: :dev},
      {:credo, ">= 0.0.0", only: :dev, runtime: false},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false},
      {:sobelow, ">= 0.0.0", only: :dev, runtime: false},
      {:git_ops, "~> 2.0.0", only: :dev},
      {:picosat_elixir, "~> 0.1.4"},
      {:nimble_options, "~> 0.3.0"},
      {:excoveralls, "~> 0.13.0", only: [:dev, :test]}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      "ash.formatter":
        "ash.formatter --extensions Ash.Dsl,Ash.Api.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia"
    ]
  end
end
