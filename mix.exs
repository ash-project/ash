defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "2.4.6"

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
      dialyzer: [plt_add_apps: [:mix, :mnesia, :earmark, :plug]],
      xref: [exclude: [:mnesia]],
      docs: docs(),
      aliases: aliases(),
      description: @description,
      source_url: "https://github.com/ash-project/ash",
      homepage_url: "https://github.com/ash-project/ash"
    ]
  end

  defp extras do
    "documentation/**/*.md"
    |> Path.wildcard()
    |> Enum.map(fn path ->
      title =
        path
        |> Path.basename(".md")
        |> String.split(~r/[-_]/)
        |> Enum.map_join(" ", &String.capitalize/1)
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

  defp groups_for_extras do
    "documentation/*"
    |> Path.wildcard()
    |> Enum.map(fn folder ->
      name =
        folder
        |> Path.basename()
        |> String.split(~r/[-_]/)
        |> Enum.map_join(" ", &String.capitalize/1)

      {name, folder |> Path.join("**") |> Path.wildcard()}
    end)
  end

  defp docs do
    # The main page in the docs
    [
      main: "get-started",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      extra_section: "GUIDES",
      extras: extras(),
      groups_for_extras: groups_for_extras(),
      groups_for_modules: [
        "Extensions & DSLs": [
          Ash.Api.Dsl,
          Ash.Resource.Dsl,
          Ash.Flow.Dsl,
          Ash.DataLayer.Ets,
          Ash.DataLayer.Mnesia,
          Ash.DataLayer.Simple,
          Ash.Notifier.PubSub,
          Ash.Policy.Authorizer,
          Ash.Registry,
          Ash.Registry.Dsl,
          Ash.Resource
        ],
        Resources: [
          Ash.Api,
          Ash.Filter.TemplateHelpers,
          Ash.Calculation,
          Ash.Resource.Calculation.Builtins,
          Ash.CodeInterface,
          Ash.DataLayer,
          Ash.Notifier,
          Ash.Notifier.Notification,
          Ash.Resource.ManualRead,
          Ash.Resource.ManualRelationship
        ],
        Queries: [
          Ash.Query,
          Ash.Resource.Preparation,
          Ash.Resource.Preparation.Builtins,
          Ash.Query.Calculation,
          Ash.Query.Aggregate
        ],
        Changesets: [
          Ash.Changeset,
          Ash.Resource.Change,
          Ash.Resource.Change.Builtins,
          Ash.Resource.Validation,
          Ash.Resource.Validation.Builtins
        ],
        Authorization: [
          Ash.Authorizer,
          Ash.Policy.Check,
          Ash.Policy.Check.Builtins,
          Ash.Policy.FilterCheck,
          Ash.Policy.SimpleCheck
        ],
        Introspection: [
          Ash.Api.Info,
          Ash.Registry.Info,
          Ash.Resource.Info,
          Ash.Flow.Info,
          Ash.Policy.Info,
          Ash.DataLayer.Ets.Info,
          Ash.DataLayer.Mnesia.Info,
          Ash.Notifier.PubSub.Info
        ],
        Utilities: [
          Ash,
          Ash.Page,
          Ash.Page.Keyset,
          Ash.Page.Offset,
          Ash.Filter,
          Ash.Filter.Runtime,
          Ash.Sort,
          Ash.CiString,
          Ash.UUID,
          Ash.NotLoaded,
          Ash.Changeset.ManagedRelationshipHelpers,
          Ash.DataLayer.Simple,
          Ash.Filter.Simple,
          Ash.Filter.Simple.Not,
          Ash.OptionsHelpers,
          Ash.Resource.Builder,
          Ash.Tracer
        ],
        Testing: [
          Ash.Generator,
          Ash.Seed,
          Ash.Test
        ],
        Flow: [
          Ash.Flow,
          Ash.Flow.Executor,
          Ash.Flow.Step,
          Ash.Flow.Chart.Mermaid,
          Ash.Flow.StepHelpers
        ],
        Errors: [
          Ash.Error,
          Ash.Error.Exception,
          Ash.Error.Stacktrace
        ],
        Types: [
          Ash.Type,
          Ash.Type.Enum,
          Ash.Type.Atom,
          Ash.Type.Binary,
          Ash.Type.Boolean,
          Ash.Type.CiString,
          Ash.Type.Date,
          Ash.Type.Decimal,
          Ash.Type.DurationName,
          Ash.Type.Float,
          Ash.Type.Function,
          Ash.Type.Integer,
          Ash.Type.Map,
          Ash.Type.NaiveDatetime,
          Ash.Type.String,
          Ash.Type.Term,
          Ash.Type.Time,
          Ash.Type.UUID,
          Ash.Type.UrlEncodedBinary,
          Ash.Type.UtcDatetime,
          Ash.Type.UtcDatetimeUsec
        ],
        Transformers: [~r/\.Transformers\./, Ash.Registry.ResourceValidations],
        Internals: ~r/.*/
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
      {:spark, "~> 0.2 and >= 0.2.10"},
      {:ecto, "~> 3.7"},
      {:ets, "~> 0.8.0"},
      {:decimal, "~> 2.0"},
      # {:picosat_elixir, path: "../picosat_elixir"},
      {:picosat_elixir, "~> 0.2"},
      {:nimble_options, "~> 0.4.0"},
      {:comparable, "~> 1.0"},
      {:jason, ">= 1.0.0"},
      {:earmark, "~> 1.4", optional: true},
      {:stream_data, "~> 0.5.0"},
      {:telemetry, "~> 1.1"},
      # Dev/Test dependencies
      {:ex_doc, "~> 0.22", only: :dev, runtime: false},
      {:ex_check, "~> 0.12.0", only: :dev},
      {:credo, ">= 0.0.0", only: :dev, runtime: false},
      {:dialyxir, ">= 0.0.0", only: :dev, runtime: false},
      {:sobelow, ">= 0.0.0", only: :dev, runtime: false},
      {:git_ops, "~> 2.5", only: :dev},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:parse_trans, "3.3.0", only: [:dev, :test], override: true},
      {:plug, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:benchee, "~> 1.1", only: [:dev, :test]}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      docs: ["docs", "ash.replace_doc_links"],
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer"
    ]
  end
end
