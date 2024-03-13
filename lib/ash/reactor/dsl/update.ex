defmodule Ash.Reactor.Dsl.Update do
  @moduledoc """
  The `update` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            async?: true,
            authorize?: nil,
            description: nil,
            domain: nil,
            initial: nil,
            inputs: [],
            name: nil,
            resource: nil,
            tenant: [],
            transform: nil,
            type: :update,
            undo_action: nil,
            undo: :never,
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
          initial: Reactor.Template.t(),
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          name: atom,
          resource: module,
          tenant: [Ash.Reactor.Dsl.Tenant.t()],
          type: :update,
          undo_action: atom,
          undo: :always | :never | :outside_transaction,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :update,
      describe: "Declares a step that will call an update action on a resource.",
      examples: [
        """
        update :publish_post, MyApp.Post, :update do
          initial input(:post)
          inputs %{
            published: value(true)
          }
          actor result(:get_user)
          tenant result(:get_organisation, [:id])
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
          initial: [
            type: Reactor.Template.type(),
            required: true,
            doc: "The record to update."
          ]
        ]
        |> Spark.Options.merge(
          Ash.Reactor.Dsl.Action.__shared_action_option_schema__(),
          "Shared action options"
        )
    }
end
