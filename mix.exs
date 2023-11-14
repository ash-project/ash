defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "2.17.1"

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
      dialyzer: [plt_add_apps: [:mix, :mnesia, :earmark, :plug, :ex_unit]],
      docs: docs(),
      aliases: aliases(),
      description: @description,
      source_url: "https://github.com/ash-project/ash",
      homepage_url: "https://github.com/ash-project/ash"
    ]
  end

  def application do
    [
      extra_applications: [:mnesia]
    ]
  end

  defp extras do
    "documentation/**/*.{md,livemd,cheatmd}"
    |> Path.wildcard()
    |> Enum.map(fn path ->
      title =
        path
        |> Path.basename(".md")
        |> Path.basename(".livemd")
        |> Path.basename(".cheatmd")
        |> String.split(~r/[-_]/)
        |> Enum.map_join(" ", &capitalize/1)
        |> case do
          "F A Q" ->
            "FAQ"

          other ->
            other
        end

      {String.to_atom(path),
       [
         title: title,
         default: title == "Get Started"
       ]}
    end)
  end

  defp capitalize(string) do
    string
    |> String.split(" ")
    |> Enum.map(fn string ->
      [hd | tail] = String.graphemes(string)
      String.capitalize(hd) <> Enum.join(tail)
    end)
  end

  defp groups_for_extras do
    [
      Tutorials: [
        "documentation/tutorials/get-started.md",
        "documentation/tutorials/philosophy.md",
        "documentation/tutorials/why-ash.md",
        ~r'documentation/tutorials'
      ],
      "How To": ~r'documentation/how_to',
      Topics: ~r'documentation/topics',
      DSLs: ~r'documentation/dsls'
    ]
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
      before_closing_head_tag: fn type ->
        if type == :html do
          """
          <script>
            if (location.hostname === "hexdocs.pm") {
              var script = document.createElement("script");
              script.src = "https://plausible.io/js/script.js";
              script.setAttribute("defer", "defer")
              script.setAttribute("data-domain", "ashhexdocs")
              document.head.appendChild(script);
            }
          </script>
          """
        end
      end,
      spark: [
        extensions: [
          %{
            module: Ash.Resource.Dsl,
            name: "Resource",
            target: "Ash.Resource",
            type: "Resource",
            default_for_target?: true
          },
          %{
            module: Ash.Api.Dsl,
            name: "Api",
            target: "Ash.Api",
            type: "Api",
            default_for_target?: true
          },
          %{
            module: Ash.DataLayer.Ets,
            name: "Ets",
            target: "Ash.Resource",
            type: "DataLayer"
          },
          %{
            module: Ash.DataLayer.Mnesia,
            name: "Mnesia",
            target: "Ash.Resource",
            type: "DataLayer"
          },
          %{
            module: Ash.Policy.Authorizer,
            name: "Policy Authorizer",
            target: "Ash.Resource",
            type: "Authorizer"
          },
          %{
            module: Ash.Flow.Dsl,
            name: "Flow",
            target: "Ash.Flow",
            type: "Flow",
            default_for_target?: true
          },
          %{
            module: Ash.Notifier.PubSub,
            name: "PubSub",
            target: "Ash.Resource",
            type: "Notifier"
          },
          %{
            module: Ash.Registry.Dsl,
            name: "Registry",
            target: "Ash.Registry",
            type: "Registry",
            default_for_target?: true
          },
          %{
            module: Ash.Registry.ResourceValidations,
            name: "Resource Validations",
            type: "Extension",
            target: "Ash.Registry"
          }
        ],
        mix_tasks: [
          Charts: [
            Mix.Tasks.Ash.GenerateFlowCharts
          ]
        ]
      ],
      groups_for_modules: [
        Extensions: [
          Ash.Api,
          Ash.Resource,
          Ash.DataLayer.Ets,
          Ash.DataLayer.Mnesia,
          Ash.DataLayer.Simple,
          Ash.Notifier.PubSub,
          Ash.Policy.Authorizer,
          Ash.Registry
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
          Ash.Resource.ManualCreate,
          Ash.Resource.ManualUpdate,
          Ash.Resource.ManualDestroy,
          Ash.Resource.ManualRelationship,
          Ash.Resource.Attribute.Helpers
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
          Ash.Policy.FilterCheckWithContext,
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
          Ash.Expr,
          Ash.Page,
          Ash.Page.Keyset,
          Ash.Page.Offset,
          Ash.Filter,
          Ash.Filter.Runtime,
          Ash.Sort,
          Ash.CiString,
          Ash.Union,
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
        Types: [
          "Ash.Type",
          ~r/Ash.Type\./
        ],
        Errors: [
          Ash.Error,
          ~r/Ash.Error\./
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
      {:spark, "~> 1.1 and >= 1.1.50"},
      {:ecto, "~> 3.7"},
      {:ets, "~> 0.8"},
      {:decimal, "~> 2.0"},
      {:picosat_elixir, "~> 0.2"},
      {:comparable, "~> 1.0"},
      {:jason, ">= 1.0.0"},
      {:stream_data, "~> 0.6"},
      {:telemetry, "~> 1.1"},
      {:plug, ">= 0.0.0", optional: true},
      {:earmark, "~> 1.4"},
      # Dev/Test dependencies
      {:ex_doc, github: "elixir-lang/ex_doc", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.12", only: [:dev, :test]},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:sobelow, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:git_ops, "~> 2.5", only: [:dev, :test]},
      {:mix_test_watch, "~> 1.0", only: [:dev, :test], runtime: false},
      {:benchee, "~> 1.1", only: [:dev, :test]},
      {:doctor, "~> 0.21", only: [:dev, :test]}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      docs: [
        "spark.cheat_sheets",
        "docs",
        "spark.replace_doc_links",
        "spark.cheat_sheets_in_search"
      ],
      "spark.cheat_sheets_in_search":
        "spark.cheat_sheets_in_search --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer",
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer",
      "spark.cheat_sheets":
        "spark.cheat_sheets --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer"
    ]
  end
end
