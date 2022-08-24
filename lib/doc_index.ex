defmodule Ash.DocIndex do
  @moduledoc """
  Some documentation about Ash.
  """

  use Spark.DocIndex,
    guides_from: [
      "documentation/**/*.md"
    ]

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
  @spec code_modules :: [{String.t(), list(module)}]
  def code_modules do
    [
      {"Resources",
       [
         Ash.Api,
         Ash.Resource.Info,
         Ash.Resource.Change,
         Ash.Resource.Change.Builtins,
         Ash.Resource.Validation,
         Ash.Resource.Validation.Builtins,
         Ash.Resource.Preparation,
         Ash.Resource.Preparation.Builtins,
         Ash.Filter.TemplateHelpers,
         Ash.Calculation,
         Ash.Resource.Calculation.Builtins,
         Ash.CodeInterface,
         Ash.Changeset,
         Ash.Query
       ]},
      {"Utilities",
       [
         Ash.Filter,
         Ash.Sort
       ]},
      {"Errors",
       [
         Ash.Error
       ]},
      {"Types",
       [
         Ash.Type,
         Ash.Type.Enum,
         Ash.Type.Atom,
         Ash.Type.Binary,
         Ash.Type.Boolean,
         Ash.Type.CiString,
         Ash.CiString,
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
       ]},
      {"DSLs and Extensions",
       [
         Spark.Dsl.Entity,
         Spark.Dsl.Extension,
         Spark.Dsl.Section,
         Spark.Dsl.Transformer
       ]},
      {"Documentation",
       [
         Spark.DocIndex
       ]}
    ]
  end
end
