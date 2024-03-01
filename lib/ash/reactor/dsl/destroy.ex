defmodule Ash.Reactor.Dsl.Destroy do
  @moduledoc """
  The `destroy` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action: nil,
            action_step?: true,
            actor: [],
            api: nil,
            async?: true,
            authorize?: nil,
            description: nil,
            initial: nil,
            inputs: [],
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
          action: atom,
          action_step?: true,
          actor: [Ash.Reactor.Dsl.Actor.t()],
          api: Ash.Api.t(),
          async?: boolean,
          authorize?: boolean | nil,
          description: String.t() | nil,
          name: atom,
          initial: Reactor.Template.t(),
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          resource: module,
          return_destroyed?: boolean,
          tenant: [Ash.Reactor.Dsl.Tenant.t()],
          type: :destroy,
          undo_action: atom,
          undo: :always | :never | :outside_transaction,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :destroy,
      describe: "Declares a step that will call a destroy action on a resource.",
      examples: [
        """
        destroy :delete_post, MyApp.Post, :destroy do
          initial input(:post)
          actor result(:get_user)
          tenant result(:get_organisation, [:id])
        end
        """
      ],
      no_depend_modules: [:api, :resource],
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
      schema: [
        action: [
          type: :atom,
          required: false,
          doc: """
          The name of the action to call on the resource.
          """
        ],
        api: [
          type: {:spark, Ash.Api},
          required: false,
          doc:
            "The API to use when calling the action.  Defaults to the API set in the `ash` section."
        ],
        async?: [
          type: :boolean,
          required: false,
          default: true,
          doc:
            "When set to true the step will be executed asynchronously via Reactor's `TaskSupervisor`."
        ],
        authorize?: [
          type: {:or, [:boolean, nil]},
          required: false,
          default: nil,
          doc: "Explicitly enable or disable authorization for the action."
        ],
        description: [
          type: :string,
          required: false,
          doc: "A description for the step"
        ],
        initial: [
          type: Reactor.Template.type(),
          required: true,
          doc: "The record to update."
        ],
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for the step.

          This is used when choosing the return value of the Reactor and for
          arguments into other steps.
          """
        ],
        resource: [
          type: {:spark, Ash.Resource},
          required: true,
          doc: """
          The resource to call the action on.
          """
        ],
        return_destroyed?: [
          type: :boolean,
          default: false,
          required: false,
          doc: "Whether or not the step should return the destroyed record upon completion."
        ],
        undo_action: [
          type: :atom,
          required: false,
          doc: """
          The name of the action to call on the resource when the step is undone.
          """
        ],
        undo: [
          type: {:in, [:always, :never, :outside_transaction]},
          required: false,
          default: :never,
          doc: """
          What to do when the reactor is undoing it's work?

          * `always` - The undo action will always be run.
          * `never` - The action will never be undone.
          * `outside_transaction` - The action will only be undone if not running inside a transaction.
          """
        ]
      ]
    }
end
