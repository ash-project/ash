# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.MixProject do
  @moduledoc false
  use Mix.Project

  @description """
  A declarative, extensible framework for building Elixir applications.
  """

  @version "3.9.0"

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
      # Workaround for Elixir dialyzer opaque type bug: https://github.com/elixir-lang/elixir/issues/14837#issuecomment-3452772021
      dialyzer: [
        plt_add_apps: [:mix, :mnesia, :plug, :ex_unit, :stream_data],
        flags: [:no_opaque]
      ],
      docs: &docs/0,
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
        {"documentation/dsls/DSL-Ash.Resource.md",
         search_data: Spark.Docs.search_data_for(Ash.Resource.Dsl)},
        {"documentation/dsls/DSL-Ash.Domain.md",
         search_data: Spark.Docs.search_data_for(Ash.Domain.Dsl)},
        {"documentation/dsls/DSL-Ash.Notifier.PubSub.md",
         search_data: Spark.Docs.search_data_for(Ash.Notifier.PubSub)},
        {"documentation/dsls/DSL-Ash.Policy.Authorizer.md",
         search_data: Spark.Docs.search_data_for(Ash.Policy.Authorizer)},
        {"documentation/dsls/DSL-Ash.DataLayer.Ets.md",
         search_data: Spark.Docs.search_data_for(Ash.DataLayer.Ets)},
        {"documentation/dsls/DSL-Ash.DataLayer.Mnesia.md",
         search_data: Spark.Docs.search_data_for(Ash.DataLayer.Mnesia)},
        {"documentation/dsls/DSL-Ash.Reactor.md",
         search_data: Spark.Docs.search_data_for(Ash.Reactor)},
        {"documentation/dsls/DSL-Ash.TypedStruct.md",
         search_data: Spark.Docs.search_data_for(Ash.TypedStruct.Dsl)},
        "documentation/tutorials/get-started.md",
        "documentation/topics/about_ash/what-is-ash.md",
        "documentation/topics/about_ash/design-principles.md",
        {".github/CONTRIBUTING.md", title: "Contributing to Ash"},
        "documentation/topics/about_ash/alternatives.md",
        "documentation/topics/development/project-structure.md",
        "documentation/topics/development/working-with-llms.md",
        "documentation/topics/development/generators.md",
        "documentation/topics/development/error-handling.md",
        "documentation/topics/development/testing.md",
        "documentation/topics/development/development-utilities.md",
        "documentation/topics/development/backwards-compatibility-config.md",
        "documentation/topics/development/upgrading-to-3.0.md",
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
        "documentation/topics/advanced/manual-installation.md",
        "documentation/topics/advanced/multi-step-actions.md",
        "documentation/topics/advanced/reactor.md",
        "documentation/topics/advanced/monitoring.md",
        "documentation/topics/advanced/pagination.livemd",
        "documentation/topics/advanced/combination-queries.md",
        "documentation/topics/advanced/timeouts.md",
        "documentation/topics/advanced/multitenancy.md",
        "documentation/topics/advanced/writing-extensions.md",
        "documentation/moved/upgrade.md",
        "documentation/topics/security/actors-and-authorization.md",
        "documentation/topics/security/sensitive-data.md",
        "documentation/topics/security/policies.md",
        "documentation/topics/reference/glossary.md",
        "documentation/topics/reference/expressions.md",
        "documentation/how-to/write-queries.livemd",
        "documentation/how-to/polymorphic-relationships.livemd",
        "documentation/how-to/test-resources.livemd",
        "documentation/how-to/authorize-access-to-resources.livemd",
        "documentation/how-to/encrypt-attributes.livemd",
        "documentation/how-to/prevent-concurrent-writes.livemd",
        "documentation/how-to/wrap-external-apis.livemd",
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
          ".github/CONTRIBUTING.md",
          "CHANGELOG.md"
        ],
        Development: ~r"documentation/topics/development",
        Reference: [
          ~r"documentation/topics/reference",
          ~r"documentation/dsls"
        ],
        Resources: ~r"documentation/topics/resources",
        Actions: ~r"documentation/topics/actions",
        Security: ~r"documentation/topics/security",
        Advanced: ~r"documentation/topics/advanced",
        "How To": ~r"documentation/how-to",
        Moved: [
          ~r"documentation/moved"
        ]
      ],
      skip_undefined_reference_warnings_on: [
        "CHANGELOG.md",
        ".github/CONTRIBUTING.md",
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
        Ash.Policy.Check,
        Ash.Type
      ],
      before_closing_head_tag: fn type ->
        if type == :html do
          """
          <meta name="exdoc:autocomplete-limit" content="25">
          <style>
            .livebook-badge-container + pre {
              display: none;
            }
            .livebook-badge-container + pre + pre {
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
      redirects: %{"contributing-to-ash" => "contributing"},
      groups_for_modules: [
        "Core API": [
          Ash,
          Ash.Query,
          Ash.Changeset,
          Ash.ActionInput
        ],
        Resources: [
          Ash.Resource.Calculation,
          Ash.Resource.Calculation.Builtins,
          Ash.CodeInterface,
          Ash.Notifier,
          Ash.Notifier.Notification,
          Ash.Resource.ManualRead,
          Ash.Resource.ManualCreate,
          Ash.Resource.ManualUpdate,
          Ash.Resource.ManualDestroy,
          Ash.Resource.ManualRelationship,
          Ash.Domain
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
          Ash.TypedStruct.Info
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
        Builtins: [
          ~r/Ash.Resource.Validation/,
          ~r/Ash.Resource.Change/,
          ~r/Ash.Policy.Check/
        ],
        Tracing: [
          Ash.Tracer,
          Ash.Tracer.Simple,
          Ash.Tracer.Simple.Span
        ],
        Utilities: [
          Ash.BulkResult,
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
          ~r/^Ash.SatSolver(\.|$)/
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
        "DSL Structs": [
          ~r/Ash.Resource.Relationships/,
          ~r/Ash.Resource.Calculation/,
          ~r/Ash.Resource.Interface/,
          ~r/Ash.Resource.Identity/,
          ~r/Ash.Resource.Attribute/,
          ~r/Ash.Resource.Attribute/,
          ~r/Ash.Resource.Attribute/,
          ~r/Ash.Resource.Aggregate/,
          ~r/Ash.Resource.Actions/,
          Ash.Mix.Tasks.Helpers,
          Ash.Policy.FieldPolicy,
          Ash.Policy.Policy,
          Ash.Notifier.PubSub.Publication
        ],
        "Data Layers": [
          Ash.DataLayer.Ets,
          Ash.DataLayer.Mnesia,
          Ash.DataLayer.Simple
        ],
        Other: ~r/.*/
      ]
    ]
  end

  defp package do
    [
      maintainers: [
        "Zach Daniel <zach@zachdaniel.dev>"
      ],
      licenses: ["MIT"],
      files: ~w(lib .formatter.exs mix.exs README* LICENSE*
      CHANGELOG* usage-rules.md),
      links: %{
        "GitHub" => "https://github.com/ash-project/ash",
        "Changelog" => "https://github.com/ash-project/ash/blob/main/CHANGELOG.md",
        "Discord" => "https://discord.gg/HTHRaaVPUc",
        "Website" => "https://ash-hq.org",
        "Forum" => "https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum",
        "REUSE Compliance" => "https://api.reuse.software/info/github.com/ash-project/ash"
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
      {:usage_rules, "~> 0.1", only: [:dev]},
      # DSLs
      {:spark, "~> 2.3 and >= 2.3.3"},
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
      {:reactor, "~> 0.11"},
      # Used for Ash.PlugHelpers
      {:plug, ">= 0.0.0", optional: true},
      # Used for aggregatable and standardized exceptions
      {:splode, "~> 0.2 and >= 0.2.6"},
      # Testing Utilities
      {:stream_data, "~> 1.0"},

      # SAT Solvers
      {:crux, "~> 0.1 and >= 0.1.2"},
      {:picosat_elixir, "~> 0.2", optional: true},
      {:simple_sat, "~> 0.1 and >= 0.1.1", optional: true},

      # Code Generators
      {:igniter, "~> 0.6 and >= 0.6.29", optional: true},

      # Dev/Test dependencies
      {:eflame, "~> 1.0", only: [:dev, :test]},
      {:ex_doc, "~> 0.37", only: [:dev, :test], runtime: false},
      {:makeup_diff, "~> 0.1.0", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.12", only: [:dev, :test]},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:mimic, "~> 2.0", only: [:test]},
      {:sobelow, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:git_ops, "~> 2.5", only: [:dev, :test]},
      {:mix_audit, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:mix_test_watch, "~> 1.0", only: [:dev, :test], runtime: false},
      {:benchee, "~> 1.1", only: [:dev, :test]},
      {:tz, "~> 0.28", only: [:test]}
    ]
  end

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      docs: [
        "ash.install --scribe documentation/topics/advanced/manual-installation.md",
        "spark.cheat_sheets",
        fn _argv ->
          File.mkdir_p!("doc/documentation/assets/images")
          File.cp_r!("documentation/assets/images", "doc/documentation/assets/images")
        end,
        "docs",
        "spark.replace_doc_links"
      ],
      format: "format --migrate",
      "spark.formatter":
        "spark.formatter --extensions Ash.Resource.Dsl,Ash.Domain.Dsl,Ash.TypedStruct.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor",
      "spark.cheat_sheets":
        "spark.cheat_sheets --extensions Ash.Resource.Dsl,Ash.Domain.Dsl,Ash.TypedStruct.Dsl,Ash.DataLayer.Ets,Ash.DataLayer.Mnesia,Ash.Notifier.PubSub,Ash.Policy.Authorizer,Ash.Reactor"
    ]
  end
end
