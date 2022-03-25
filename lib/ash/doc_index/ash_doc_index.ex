defmodule Ash.DocIndex.AshDocIndex do
  @moduledoc "Doc index for Ash"
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
        default_for_target?: true
      },
      %{
        module: Ash.Api.Dsl,
        name: "Api",
        target: "Ash.Api",
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
        default_for_target?: true
      },
      %{
        module: Ash.Flow.Dsl,
        name: "Flow",
        target: "Ash.Flow",
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
        default_for_target?: true
      },
      %{
        module: Ash.Registry.ResourceValidations,
        name: "Resource Validations",
        target: "Ash.Registry"
      }
    ]
  end
end
