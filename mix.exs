defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "2.0.0-pre.1"

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

  defp extras() do
    "documentation/**/*.md"
    |> Path.wildcard()
    |> Enum.map(fn path ->
      title =
        path
        |> Path.basename(".md")
        |> String.split(~r/[-_]/)
        |> Enum.map(&String.capitalize/1)
        |> Enum.join(" ")
        |> case do
          "F A Q" ->
            "FAQ"

          other ->
            other
        end

      {String.to_atom(path),
       [
         title: title
       ]}
    end)
  end

  defp groups_for_extras() do
    "documentation/*"
    |> Path.wildcard()
    |> Enum.map(fn folder ->
      name =
        folder
        |> Path.basename()
        |> String.split(~r/[-_]/)
        |> Enum.map(&String.capitalize/1)
        |> Enum.join(" ")

      {name, folder |> Path.join("**") |> Path.wildcard()}
    end)
  end

  defp docs do
    # The main page in the docs
    [
      main: "quick-start",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      extra_section: "GUIDES",
      extras: extras(),
      groups_for_extras: groups_for_extras(),
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
          Ash.SatSolver
        ],
        comparable: ~r/Comparable/
      ]
    ]
  end

  defp package do
    [
      name: :ash,
      licenses: ["MIT"],
      files: ~w(lib .formatter.exs mix.exs README* LICENSE*
      CHANGELOG* documentation),
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
      {:spark, "~> 0.1 and >= 0.1.9"},
      {:ecto, "~> 3.7"},
      {:ets, "~> 0.8.0"},
      {:decimal, "~> 2.0"},
      {:picosat_elixir, "~> 0.2"},
      {:nimble_options, "~> 0.4.0"},
      {:comparable, "~> 1.0"},
      {:jason, ">= 1.0.0"},
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
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer"
    ]
  end
end
