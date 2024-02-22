defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A resource declaration and interaction library. Built with pluggable data layers, and
  designed to be used by multiple front ends.
  """

  @version "2.21.1"

  def project do
    [
      app: :ash,
      version: @version,
      elixir: "~> 1.11",
      consolidate_protocols: Mix.env() == :prod,
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

  defp docs do
    [
      main: "get-started",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      extra_section: "GUIDES",
      extras: [
        "documentation/tutorials/get-started.md",
        "documentation/tutorials/why-ash.md",
        "documentation/tutorials/philosophy.md",
        "documentation/tutorials/using-hexdocs.md",
        "documentation/tutorials/extending-resources.md",
        "documentation/how_to/contribute.md",
        "documentation/how_to/define-idiomatic-actions.md",
        "documentation/how_to/defining-manual-relationships.md",
        "documentation/how_to/handle-errors.md",
        "documentation/how_to/structure-your-project.md",
        "documentation/how_to/upgrade.md",
        "documentation/how_to/use-without-data-layers.md",
        "documentation/how_to/validate-changes.md",
        "documentation/how_to/auto-format-code.md",
        "documentation/topics/actions.md",
        "documentation/topics/aggregates.md",
        "documentation/topics/atomics.md",
        "documentation/topics/attributes.md",
        "documentation/topics/bulk-actions.md",
        "documentation/topics/calculations.md",
        "documentation/topics/code-interface.md",
        "documentation/topics/constraints.md",
        "documentation/topics/development-utilities.md",
        "documentation/topics/embedded-resources.md",
        "documentation/topics/expressions.md",
        "documentation/topics/flows.md",
        "documentation/topics/glossary.md",
        "documentation/topics/identities.md",
        "documentation/topics/managing-relationships.md",
        "documentation/topics/manual-actions.md",
        "documentation/topics/monitoring.md",
        "documentation/topics/multitenancy.md",
        "documentation/topics/notifiers.md",
        "documentation/topics/pagination.md",
        "documentation/topics/phoenix.md",
        "documentation/topics/policies.md",
        "documentation/topics/pub_sub.md",
        "documentation/topics/reactor.md",
        "documentation/topics/relationships.md",
        "documentation/topics/security.md",
        "documentation/topics/store-context-in-process.md",
        "documentation/topics/testing.md",
        "documentation/topics/timeouts.md",
        "documentation/topics/validations.md",
        "documentation/dsls/DSL:-Ash.Resource.md",
        "documentation/dsls/DSL:-Ash.Api.md",
        "documentation/dsls/DSL:-Ash.Notifier.PubSub.md",
        "documentation/dsls/DSL:-Ash.Policy.Authorizer.md",
        "documentation/dsls/DSL:-Ash.Flow.md",
        "documentation/dsls/DSL:-Ash.DataLayer.Ets.md",
        "documentation/dsls/DSL:-Ash.DataLayer.Mnesia.md",
        "documentation/dsls/DSL:-Ash.Reactor.md",
        "documentation/dsls/DSL:-Ash.DataLayer.Mnesia.md"
      ],
      groups_for_extras: [
        Tutorials: ~r'documentation/tutorials',
        "How To": ~r'documentation/how_to',
        Topics: ~r'documentation/topics',
        DSLs: ~r'documentation/dsls'
      ],
      nest_modules_by_prefix: [
        Ash.Error,
        Ash.Flow.Transformers,
        Ash.Policy.Authorizer,
        Ash.Api.Transformers,
        Ash.Api.Verifiers,
        Ash.Resource.Transformers,
        Ash.Resource.Verifiers,
        Ash.Query.Function,
        Ash.Query.Operator,
        Ash.Resource.Change,
        Ash.Resource.Validation,
        Ash.Policy.Check
      ],
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
      groups_for_modules: [
        Resources: [
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
        "Action Input & Interface": [
          Ash,
          Ash.Api,
          Ash.Query,
          Ash.Changeset,
          Ash.ActionInput,
          Ash.BulkResult
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
        Extensions: [
          Ash.Resource,
          Ash.DataLayer.Ets,
          Ash.DataLayer.Mnesia,
          Ash.DataLayer.Simple,
          Ash.Notifier.PubSub,
<<<<<<< HEAD
          Ash.Policy.Authorizer,
          Ash.Registry,
          Ash.Reactor
=======
          Ash.Policy.Authorizer
>>>>>>> c75c3115 (improvement!: remove registries)
        ],
        Introspection: [
          Ash.Api.Info,
          Ash.Resource.Info,
          Ash.Flow.Info,
          Ash.Policy.Info,
          Ash.DataLayer.Ets.Info,
          Ash.DataLayer.Mnesia.Info,
          Ash.Notifier.PubSub.Info,
          ~r/Ash.Api.Dsl.*/
        ],
        Utilities: [
          Ash.Expr,
          Ash.Page,
          Ash.Page.Keyset,
          Ash.Page.Offset,
          Ash.Filter,
          Ash.Filter.Runtime,
          Ash.Sort,
          Ash.CiString,
          Ash.Vector,
          Ash.Union,
          Ash.UUID,
          Ash.NotLoaded,
          Ash.ForbiddenField,
          Ash.Changeset.ManagedRelationshipHelpers,
          Ash.DataLayer.Simple,
          Ash.Filter.Simple,
          Ash.Filter.Simple.Not,
          Ash.OptionsHelpers,
          Ash.Resource.Builder,
          Ash.ProcessHelpers,
          Ash.Mix.Tasks.Helpers,
          Ash.PlugHelpers,
          Ash.SatSolver
        ],
        Visualizations: [
          Ash.Api.Info.Diagram,
          Ash.Api.Info.Livebook,
          Ash.Policy.Chart.Mermaid
        ],
        Testing: [
          Ash.Generator,
          Ash.Seed,
          Ash.Test
        ],
        Tracing: [
          Ash.Tracer,
          Ash.Tracer.Simple,
          Ash.Tracer.Simple.Span
        ],
        Flow: [
          Ash.Flow,
          Ash.Flow.Result,
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
        "DSL Transformers": [
          ~r/\.Transformers\./,
          ~r/\.Verifiers\./
        ],
        Expressions: [
          Ash.Filter.Predicate,
          Ash.Filter.Simple,
          Ash.Filter.Simple.Not,
          ~r/Ash.Query.Operator/,
          ~r/Ash.Query.Function/,
          ~r/Ash.Query.Ref/,
          Ash.Query.Not,
          Ash.Query.Call,
          Ash.Query.BooleanExpression,
          Ash.Query.Exists,
          Ash.Query.Parent
        ],
        Builtins: [
          ~r/Ash.Resource.Validation/,
          ~r/Ash.Resource.Change/,
          ~r/Ash.Policy.Check/
        ],
        Introspection: [
          ~r/Ash.Resource.Relationships/,
          ~r/Ash.Resource.Calculation/,
          ~r/Ash.Resource.Interface/,
          ~r/Ash.Resource.Identity/,
          ~r/Ash.Resource.Attribute/,
          ~r/Ash.Resource.Attribute/,
          ~r/Ash.Resource.Aggregate/,
          ~r/Ash.Resource.Actions/,
          ~r/Ash.Flow.Step/,
          ~r/Ash.Flow/,
          Ash.Mix.Tasks.Helpers,
          Ash.Policy.FieldPolicy,
          Ash.Policy.Policy,
          Ash.Notifier.PubSub.Publication
        ],
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
        GitHub: "https://github.com/ash-project/ash",
        Discord: "https://discord.gg/HTHRaaVPUc",
        Website: "https://ash-hq.org",
        Forum: "https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum"
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
      {:spark, "~> 1.1 and >= 1.1.55"},
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
      {:reactor, "~> 0.6"},

      # Dev/Test dependencies
      {:eflame, "~> 1.0", only: [:dev, :test]},
      {:ex_doc, github: "elixir-lang/ex_doc", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.12", only: [:dev, :test]},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:mimic, "~> 1.7", only: [:test]},
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
<<<<<<< HEAD
        "spark.cheat_sheets_in_search --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor",
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor",
      "spark.cheat_sheets":
        "spark.cheat_sheets --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.Registry.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor"
=======
        "spark.cheat_sheets_in_search --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer",
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer",
      "spark.cheat_sheets":
        "spark.cheat_sheets --extensions Ash.Resource.Dsl,Ash.Api.Dsl,Ash.Flow.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer"
>>>>>>> c75c3115 (improvement!: remove registries)
    ]
  end
end
