defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "1.52.0-rc.22"

  def project do
    [
      app: :ash,
      version: @version,
      elixir: "~> 1.11",
      consolidate_protocols: Mix.env() != :test,
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      deps: deps(),
      dialyzer: [plt_add_apps: [:mix, :mnesia, :earmark]],
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
        "old_documentation/introduction/getting_started.md": [
          title: "Getting Started"
        ],
        "old_documentation/introduction/getting_started_phx.md": [
          title: "Getting Started With Phoenix"
        ],
        "old_documentation/topics/expressions.md": [
          title: "Expressions"
        ],
        "old_documentation/topics/managing_relationships.md": [
          title: "Managing Relationships"
        ],
        "old_documentation/topics/resources_without_a_data_layer.md": [
          title: "Resources without a Data Layer"
        ],
        "old_documentation/topics/authorization.md": [
          title: "Authorization"
        ],
        "old_documentation/topics/identities.md": [
          title: "Identities"
        ],
        "old_documentation/topics/pagination.md": [
          title: "Pagination"
        ],
        "old_documentation/topics/validation.md": [
          title: "Validation"
        ],
        "old_documentation/topics/notifiers.md": [
          title: "Notifiers"
        ],
        "old_documentation/topics/error_handling.md": [
          title: "Error Handling"
        ],
        "old_documentation/topics/aggregates.md": [
          title: "Aggregates"
        ],
        "old_documentation/topics/calculations.md": [
          title: "Calculations"
        ],
        "old_documentation/topics/embedded_resources.md": [
          title: "Embedded Resources"
        ],
        "old_documentation/topics/multitenancy.md": [
          title: "Multitenancy"
        ],
        "old_documentation/topics/compile_times.md": [
          title: "Compile Times"
        ],
        "priv/documentation/topics/policies.md": [
          title: "Policies"
        ],
        "priv/documentation/topics/overview.md": [
          title: "Overview"
        ],
        "priv/documentation/topics/development-utilities.md": [
          title: "Dev Utilities"
        ]
      ],
      groups_for_extras: [
        Introduction: Path.wildcard("old_documentation/introduction/*.md"),
        Topics:
          Path.wildcard("old_documentation/topics/*") ++ Path.wildcard("documentation/topics/*")
      ],
      groups_for_modules: [
        entrypoint: [
          Ash,
          Ash.Api,
          Ash.Query,
          Ash.Changeset,
          Ash.Resource.Dsl,
          Ash.Api.Dsl,
          Ash.CodeInterface
        ],
        tools: [
          Ash.Filter,
          Ash.Sort
        ],
        formatting: [
          Ash.ResourceFormatter
        ],
        validations: ~r/Ash.Resource.Validation/,
        changes: ~r/Ash.Resource.Change/,
        calculations: [
          ~r/Ash.Resource.Calculation/,
          Ash.Query.Calculation,
          Ash.Calculation
        ],
        values: [
          Ash.CiString
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
        "dsl tooling": [
          Ash.Dsl
        ],
        "resource dsl transformers": ~r/Ash.Resource.Transformers/,
        "api dsl transformers": ~r/Ash.Api.Transformers/,
        "filter operators": ~r/Ash.Query.Operator/,
        "filter functions": ~r/Ash.Query.Function/,
        "query expressions": [
          Ash.Query.BooleanExpression,
          Ash.Query.Not,
          Ash.Query.Ref,
          Ash.Query.Call
        ],
        filter: ~r/Ash.Filter/,
        "resource introspection": ~r/Ash.Resource/,
        "api introspection": ~r/Ash.Api/,
        engine: [
          ~r/Ash.Engine/
        ],
        registry: [Ash.Registry],
        "registry introspection": ~r/Ash.Registry/,
        errors: [
          Ash.Error,
          Ash.Error.Stacktrace,
          Ash.Error.Exception
        ],
        flow: [
          Ash.Flow
        ],
        miscellaneous: [
          Ash.UUID,
          Ash.Changeset.ManagedRelationshipHelpers,
          Ash.NotLoaded,
          Ash.Query.Aggregate,
          Ash.Query.Type,
          Ash.SatSolver,
          Ash.OptionsHelpers
        ],
        comparable: ~r/Comparable/
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
      {:ecto, "~> 3.7"},
      {:ets, "~> 0.8.0"},
      {:decimal, "~> 2.0"},
      {:picosat_elixir, "~> 0.2"},
      {:nimble_options, "~> 0.3.5"},
      {:timex, ">= 3.0.0"},
      {:comparable, "~> 1.0"},
      {:jason, ">= 1.0.0"},
      {:sourceror, "~> 0.9"},
      {:earmark, "~> 1.4", optional: true},
      {:stream_data, "~> 0.5.0"},
      # Dev/Test dependencies
      {:ex_doc, "~> 0.22", only: :dev, runtime: false},
      {:ex_check, "~> 0.12.0", only: :dev},
      {:credo, ">= 0.0.0", only: :dev, runtime: false},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false},
      {:sobelow, ">= 0.0.0", only: :dev, runtime: false},
      {:git_ops, "~> 2.4.4", only: :dev},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:parse_trans, "3.3.0", only: [:dev, :test], override: true},
      {:elixir_sense, github: "elixir-lsp/elixir_sense", only: [:dev, :test, :docs]}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      "ash.formatter":
        "ash.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer"
    ]
  end
end
