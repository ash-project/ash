defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "1.25.1"

  def project do
    [
      app: :ash,
      version: @version,
      elixir: "~> 1.8",
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
      xref: [exclude: [:mnesia]],
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
      main: "readme",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      extra_section: "GUIDES",
      extras: [
        "README.md": [],
        "documentation/introduction/getting_started.md": [
          title: "Getting Started"
        ],
        "documentation/introduction/getting_started_phx.md": [
          title: "Getting Started with Phoenix"
        ],
        "documentation/topics/authorization.md": [],
        "documentation/topics/identities.md": [],
        "documentation/topics/pagination.md": [],
        "documentation/topics/validation.md": [],
        "documentation/topics/notifiers.md": [],
        "documentation/topics/error_handling.md": [],
        "documentation/topics/aggregates.md": [],
        "documentation/topics/calculations.md": [],
        "documentation/topics/contexts_and_domains.md": [],
        "documentation/topics/multitenancy.md": []
      ],
      groups_for_extras: [
        Introduction: Path.wildcard("documentation/introduction/*.md"),
        Topics: Path.wildcard("documentation/topics/*")
      ],
      groups_for_modules: [
        entrypoint: [
          Ash,
          Ash.Api,
          Ash.Query,
          Ash.Changeset,
          Ash.Resource.Dsl,
          Ash.Api.Dsl
        ],
        validations: ~r/Ash.Resource.Validation/,
        changes: ~r/Ash.Resource.Change/,
        calculations: [
          ~r/Ash.Resource.Calculation/,
          Ash.Query.Calculation,
          Ash.Calculation
        ],
        type: ~r/Ash.Type/,
        data_layer: ~r/Ash.DataLayer/,
        authorizer: ~r/Ash.Authorizer/,
        pagination: ~r/Ash.Page/,
        notifications: ~r/Ash.Notifier/,
        extension: [
          Ash.Dsl.Entity,
          Ash.Dsl.Extension,
          Ash.Dsl.Section,
          Ash.Dsl.Transformer
        ],
        "resource dsl": ~r/Ash.Resource.Dsl/,
        "resource dsl transformers": ~r/Ash.Resource.Transformers/,
        "api dsl transformers": ~r/Ash.Api.Transformers/,
        "api dsl": ~r/Ash.Api.Dsl/,
        "filter operators": ~r/Ash.Query.Operator/,
        "filter functions": ~r/Ash.Query.Function/,
        "query expressions": [
          Ash.Query.Expression,
          Ash.Query.Not,
          Ash.Query.Ref
        ],
        filter: ~r/Ash.Filter/,
        "resource introspection": ~r/Ash.Resource/,
        "api introspection": ~r/Ash.Api/,
        engine: [
          ~r/Ash.Engine/
        ],
        miscellaneous: [
          Ash.NotLoaded,
          Ash.Error.Stacktrace,
          Ash.Query.Aggregate
        ]
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
      {:ex_check, "~> 0.12.0", only: :dev},
      {:credo, ">= 0.0.0", only: :dev, runtime: false},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false},
      {:sobelow, ">= 0.0.0", only: :dev, runtime: false},
      {:git_ops, "~> 2.0.1", only: :dev},
      {:picosat_elixir, "~> 0.1.5"},
      {:nimble_options, "~> 0.3.0"},
      {:excoveralls, "~> 0.13.0", only: [:dev, :test]},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:parse_trans, "3.3.0", only: [:dev, :test], override: true}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      "ash.formatter":
        "ash.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub"
    ]
  end
end
