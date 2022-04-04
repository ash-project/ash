defmodule Ash.DocIndex.AshDocIndex do
  @moduledoc """
  Some documentation about Ash.
  """

  @behaviour Ash.DocIndex

  @impl true
  @spec for_library() :: String.t()
  def for_library, do: "ash"

  @impl true
  @spec extensions() :: list(Ash.DocIndex.extension())
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

  @getting_started File.read!("documentation/guides/getting-started.md")
  @overview File.read!("documentation/guides/overview.md")
  @managing_relationships File.read!("documentation/guides/managing-relationships.md")
  @upgrading File.read!("documentation/guides/upgrading.md")

  @impl true
  @spec guides() :: list(Ash.DocIndex.guide())
  def guides do
    [
      %{
        name: "Overview",
        text: @overview
      },
      %{
        name: "Getting Started",
        text: @getting_started
      },
      %{
        name: "Managing Relationships",
        text: @managing_relationships
      },
      %{
        name: "Upgrading",
        text: @upgrading
      }
    ]
  end

  @impl true
  @spec code_modules :: list(module())
  def code_modules do
    [
      Ash.Resource.Info,
      Ash.Api,
      Ash.Type,
      Ash.Error,
      Ash.CiString,
      Ash.Changeset,
      Ash.Query,
      Ash.Filter,
      Ash.Sort,
      Ash.Resource.Change,
      Ash.Resource.Change.Builtins,
      Ash.Calculation,
      Ash.CodeInterface,
      Ash.Dsl.Entity,
      Ash.Dsl.Extension,
      Ash.Dsl.Section,
      ASh.Dsl.Transformer
    ]
  end
end
