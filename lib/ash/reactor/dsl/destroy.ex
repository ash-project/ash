defmodule Ash.Reactor.Dsl.Destroy do
  @moduledoc """
  The `destroy` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            async?: true,
            authorize?: nil,
            context: nil,
            description: nil,
            domain: nil,
            guards: [],
            initial: nil,
            inputs: [],
            load: nil,
            name: nil,
            resource: nil,
            return_destroyed?: false,
            tenant: [],
            transform: nil,
            type: :destroy,
            undo_action: nil,
            undo: :never,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action_step?: true,
          action: atom,
          actor: nil | Ash.Reactor.Dsl.Actor.t(),
          async?: boolean,
          authorize?: boolean | nil,
          context: nil | Ash.Reactor.Dsl.Context.t(),
          description: String.t() | nil,
          domain: Ash.Domain.t(),
          guards: [Reactor.Guard.Build.t()],
          initial: Reactor.Template.t(),
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          load: nil | Ash.Reactor.Dsl.ActionLoad.t(),
          name: atom,
          resource: module,
          return_destroyed?: boolean,
          tenant: nil | Ash.Reactor.Dsl.Tenant.t(),
          type: :destroy,
          undo_action: atom,
          undo: :always | :never | :outside_transaction,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :destroy,
      describe: """
      Declares a step that will call a destroy action on a resource.

      #{Ash.Reactor.Dsl.Action.__shared_undo_docs__()}
      """,
      examples: [
        """
        destroy :delete_post, MyApp.Post, :destroy do
          initial input(:post)
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
        context: [Ash.Reactor.Dsl.Context.__entity__()],
        guards: [Reactor.Dsl.Guard.__entity__(), Reactor.Dsl.Where.__entity__()],
        inputs: [Ash.Reactor.Dsl.Inputs.__entity__()],
        load: [Ash.Reactor.Dsl.ActionLoad.__entity__()],
        tenant: [Ash.Reactor.Dsl.Tenant.__entity__()],
        wait_for: [Reactor.Dsl.WaitFor.__entity__()]
      ],
      singleton_entity_keys: [:actor, :context, :load, :tenant],
      recursive_as: :steps,
      schema:
        [
          initial: [
            type: Reactor.Template.type(),
            required: true,
            doc: "The record to update."
          ],
          return_destroyed?: [
            type: :boolean,
            default: false,
            required: false,
            doc: "Whether or not the step should return the destroyed record upon completion."
          ]
        ]
        |> Spark.Options.merge(
          Ash.Reactor.Dsl.Action.__shared_action_option_schema__(),
          "Shared action options"
        )
    }
end
