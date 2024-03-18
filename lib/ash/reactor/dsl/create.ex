defmodule Ash.Reactor.Dsl.Create do
  @moduledoc """
  The `create` entity for the `Ash.Reactor` reactor extension.
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
            type: :create,
            undo_action: nil,
            undo: :never,
            upsert_identity: nil,
            upsert?: false,
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
          initial: nil | Ash.Resource.t() | Reactor.Template.t(),
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          name: atom,
          resource: module,
          tenant: [Ash.Reactor.Dsl.Tenant.t()],
          type: :create,
          undo_action: atom,
          undo: :always | :never | :outside_transaction,
          upsert_identity: nil | atom,
          upsert?: boolean,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :create,
      describe: """
      Declares a step that will call a create action on a resource.

      #{Ash.Reactor.Dsl.Action.__shared_undo_docs__()}
      """,
      examples: [
        """
        create :create_post, MyApp.Post, :create do
          inputs %{
            title: input(:post_title),
            author_id: result(:get_user, [:id])
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
            type: {:or, [nil, {:spark, Ash.Resource}, Reactor.Template.type()]},
            required: false,
            doc: "The initial value passed into the action."
          ],
          upsert_identity: [
            type: :atom,
            required: false,
            doc: "The identity to use for the upsert"
          ],
          upsert?: [
            type: :boolean,
            required: false,
            default: false,
            doc: "Whether or not this action should be executed as an upsert."
          ]
        ]
        |> Spark.Options.merge(
          Ash.Reactor.Dsl.Action.__shared_action_option_schema__(),
          "Shared action options"
        )
    }
end
