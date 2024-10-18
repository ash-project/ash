defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A declarative, extensible framework for building Elixir applications.
  """

  @version "3.4.33"

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
      dialyzer: [plt_add_apps: [:mix, :mnesia, :plug, :ex_unit]],
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
      main: "readme",
      source_ref: "v#{@version}",
      logo: "logos/small-logo.png",
      extra_section: "GUIDES",
      extras: [
        {"README.md", title: "Home"},
        "documentation/tutorials/get-started.md",
        "documentation/topics/about_ash/what-is-ash.md",
        "documentation/topics/about_ash/design-principles.md",
        "documentation/topics/about_ash/contributing-to-ash.md",
        "documentation/topics/about_ash/alternatives.md",
        "documentation/topics/resources/domains.md",
        "documentation/topics/resources/attributes.md",
        "documentation/topics/resources/relationships.md",
        "documentation/topics/resources/calculations.md",
        "documentation/topics/resources/aggregates.md",
        "documentation/topics/resources/validations.md",
        "documentation/topics/resources/changes.md",
        "documentation/topics/resources/preparations.md",
        "documentation/topics/resources/code-interfaces.md",
        "documentation/topics/resources/embedded-resources.md",
        "documentation/topics/resources/identities.md",
        "documentation/topics/resources/notifiers.md",
        "documentation/topics/actions/actions.md",
        "documentation/topics/actions/read-actions.md",
        "documentation/topics/actions/create-actions.md",
        "documentation/topics/actions/update-actions.md",
        "documentation/topics/actions/destroy-actions.md",
        "documentation/topics/actions/generic-actions.md",
        "documentation/topics/actions/manual-actions.md",
        "documentation/topics/advanced/reactor.md",
        "documentation/topics/advanced/monitoring.md",
        "documentation/topics/advanced/pagination.livemd",
        "documentation/topics/advanced/timeouts.md",
        "documentation/topics/advanced/multitenancy.md",
        "documentation/topics/advanced/writing-extensions.md",
        "documentation/topics/development/project-structure.md",
        "documentation/topics/development/generators.md",
        "documentation/topics/development/error-handling.md",
        "documentation/topics/development/testing.md",
        "documentation/topics/development/development-utilities.md",
        "documentation/topics/development/upgrading-to-3.0.md",
        "documentation/moved/upgrade.md",
        "documentation/topics/security/actors-and-authorization.md",
        "documentation/topics/security/sensitive-data.md",
        "documentation/topics/security/policies.md",
        "documentation/topics/reference/glossary.md",
        "documentation/topics/reference/expressions.md",
        "documentation/how-to/test-resources.livemd",
        "documentation/how-to/authorize-access-to-resources.livemd",
        "documentation/how-to/encrypt-attributes.livemd",
        "documentation/how-to/prevent-concurrent-writes.livemd",
        "documentation/how-to/wrap-external-apis.livemd",
        "documentation/dsls/DSL-Ash.Resource.md",
        "documentation/dsls/DSL-Ash.Domain.md",
        "documentation/dsls/DSL-Ash.Notifier.PubSub.md",
        "documentation/dsls/DSL-Ash.Policy.Authorizer.md",
        "documentation/dsls/DSL-Ash.DataLayer.Ets.md",
        "documentation/dsls/DSL-Ash.DataLayer.Mnesia.md",
        "documentation/dsls/DSL-Ash.Reactor.md",
        "CHANGELOG.md"
      ],
      groups_for_extras: [
        "Start Here": [
          "readme.md",
          "documentation/tutorials/get-started.md"
        ],
        Tutorials: ~r"documentation/tutorials",
        "About Ash": [
          ~r"documentation/topics/about_ash",
          "CHANGELOG.md"
        ],
        Resources: ~r"documentation/topics/resources",
        Actions: ~r"documentation/topics/actions",
        Security: ~r"documentation/topics/security",
        Development: ~r"documentation/topics/development",
        Advanced: ~r"documentation/topics/advanced",
        "How To": ~r"documentation/how-to",
        Reference: [
          ~r"documentation/topics/reference",
          ~r"documentation/dsls"
        ],
        Moved: [
          ~r"documentation/moved"
        ]
      ],
      skip_undefined_reference_warnings_on: [
        "CHANGELOG.md",
        "documentation/topics/reference/glossary.md",
        "documentation/topics/development/upgrading-to-3.0.md"
      ],
      nest_modules_by_prefix: [
        Ash.Error,
        Ash.Policy.Authorizer,
        Ash.Domain.Transformers,
        Ash.Domain.Verifiers,
        Ash.Resource.Transformers,
        Ash.Resource.Verifiers,
        Ash.Query.Function,
        Ash.Query.Operator,
        Ash.Resource.Validation,
        Ash.Resource.Change,
        Ash.Policy.Check
      ],
      before_closing_head_tag: fn type ->
        if type == :html do
          """
          <style>
            .livebook-badge-container + pre {
              display: none;
            }
          </style>
          <script>
            if (location.hostname === "hexdocs.pm") {
              var script = document.createElement("script");
              script.src = "https://plausible.io/js/script.js";
              script.setAttribute("defer", "defer")
              script.setAttribute("data-domain", "ashhexdocs")
              document.head.appendChild(script);
            }
          </script>
          <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
          <script>mermaid.initialize({startOnLoad: true})</script>
          """
        end
      end,
      groups_for_modules: [
        Resources: [
          Ash.Resource.Calculation,
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
          Ash.Domain,
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
        Changes: [
          Ash.Changeset,
          Ash.Resource.Change,
          Ash.Resource.Change.Builtins
        ],
        Validations: [
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
        Extensions: [
          Ash.Resource,
          Ash.DataLayer.Ets,
          Ash.DataLayer.Mnesia,
          Ash.DataLayer.Simple,
          Ash.Notifier.PubSub,
          Ash.Policy.Authorizer,
          Ash.Reactor
        ],
        Introspection: [
          Ash.Domain.Info,
          Ash.Resource.Info,
          Ash.Policy.Info,
          Ash.DataLayer.Ets.Info,
          Ash.DataLayer.Mnesia.Info,
          Ash.Notifier.PubSub.Info,
          ~r/Ash.Domain.Dsl.*/
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
          Ash.UUIDv7,
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
          Ash.Domain.Info.Diagram,
          Ash.Domain.Info.Livebook,
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
      # DSLs
      {:spark, "~> 2.1 and >= 2.2.29"},
      # Ash resources are backed by ecto scheams
      {:ecto, "~> 3.7"},
      # Used by the ETS data layer
      {:ets, "~> 0.8"},
      # Data & types
      {:decimal, "~> 2.0"},
      {:jason, ">= 1.0.0"},
      # Observability
      {:telemetry, "~> 1.1"},
      # Used for providing Ash.Reactor, will be used more in the future
      {:reactor, "~> 0.9"},
      # Used for Ash.PlugHelpers
      {:plug, ">= 0.0.0", optional: true},
      # Used for aggregatable and standardized exceptions
      {:splode, "~> 0.2"},
      # Testing Utilities
      {:stream_data, "~> 1.0"},

      # SAT Solvers
      {:picosat_elixir, "~> 0.2", optional: true},
      {:simple_sat, "~> 0.1 and >= 0.1.1", optional: true},

      # Code Generators
      {:igniter, "~> 0.3 and >= 0.3.61"},

      # IO Utilities
      {:owl, "~> 0.11"},

      # Dev/Test dependencies
      {:eflame, "~> 1.0", only: [:dev, :test]},
      {:ex_doc, "~> 0.32", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.12", only: [:dev, :test]},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:mimic, "~> 1.7", only: [:test]},
      {:sobelow, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:git_ops, "~> 2.5", only: [:dev, :test]},
      {:mix_audit, ">= 0.0.0", only: [:dev, :test], runtime: false},
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
        "spark.cheat_sheets_in_search --extensions Ash.Resource.Dsl,Ash.Domain.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor",
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Domain.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor",
      "spark.cheat_sheets":
        "spark.cheat_sheets --extensions Ash.Resource.Dsl,Ash.Domain.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor"
    ]
  end
end
