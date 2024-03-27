defmodule Ash.Reactor.Dsl.ReadOne do
  @moduledoc """
  The `read_one` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            async?: true,
            authorize?: nil,
            description: nil,
            domain: nil,
            fail_on_not_found?: nil,
            inputs: [],
            name: nil,
            resource: nil,
            tenant: [],
            transform: nil,
            type: :read,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action_step?: true,
          action: atom,
          actor: [Ash.Reactor.Dsl.Actor.t()],
          async?: boolean,
          authorize?: boolean | nil,
          description: String.t() | nil,
          domain: Ash.Domain.t(),
          fail_on_not_found?: boolean,
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          name: atom,
          resource: module,
          tenant: [Ash.Reactor.Dsl.Tenant.t()],
          type: :create,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :read_one,
      describe:
        "Declares a step that will call a read action on a resource returning a single record.",
      examples: [
        """
        read_one :post_by_id, MyApp.Post, :read do
          inputs %{id: input(:post_id)}
        end
        """
      ],
      no_depend_modules: [:domain, :resource],
      target: __MODULE__,
      args: [:name, :resource, {:optional, :action}],
      identifier: :name,
      imports: [Reactor.Dsl.Argument],
      entities: [
        actor: [Ash.Reactor.Dsl.Actor.__entity__()],
        inputs: [Ash.Reactor.Dsl.Inputs.__entity__()],
        tenant: [Ash.Reactor.Dsl.Tenant.__entity__()],
        wait_for: [Reactor.Dsl.WaitFor.__entity__()]
      ],
      singleton_entity_keys: [:actor, :tenant],
      recursive_as: :steps,
      schema:
        [
          fail_on_not_found?: [
            type: :boolean,
            required: false,
            default: false,
            doc: "When set to true the step will fail if the resource is not found."
          ]
        ]
        |> Spark.Options.merge(
          Ash.Reactor.Dsl.Action.__shared_action_option_schema__(false),
          "Shared action options"
        )
    }
end
