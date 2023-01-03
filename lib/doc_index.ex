defmodule Ash.DocIndex do
  @moduledoc false

  use Spark.DocIndex,
    guides_from: [
      "documentation/**/*.md"
    ],
    guide_order: %{
      "Tutorials" => [
        "get-started.md",
        "philosophy.md",
        "why-ash.md"
      ]
    }

  @impl true
  @spec for_library() :: String.t()
  def for_library, do: "ash"

  @impl true
  @spec extensions() :: list(Spark.DocIndex.extension())
  def extensions do
    [
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
    ]
  end

  @impl true
  @spec mix_tasks :: [{String.t(), list(module)}]
  def mix_tasks do
    [
      {
        "Charts",
        [
          Mix.Tasks.Ash.GenerateFlowCharts
        ]
      }
    ]
  end

  @impl true
  @spec code_modules :: [{String.t(), list(module)}]
  def code_modules do
    [
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
      Expressions: [
        Ash.Expr
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
        Ash.Seed
      ],
      Flow: [
        Ash.Flow,
        Ash.Flow.Step,
        Ash.Flow.Executor,
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
      ]
    ]
  end
end
