defmodule Ash.DocIndex do
  @moduledoc "A module for configuring how a library is rendered in ash_hq"
  @type extension :: %{
          optional(:module) => module,
          optional(:target) => String.t(),
          optional(:default_for_target?) => boolean,
          optional(:name) => String.t(),
          optional(:type) => String.t()
        }

  @callback extensions() :: list(extension())

  @behaviour __MODULE__

  @impl true
  @spec extensions() :: list(extension)
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
        module: Ash.Registry,
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
