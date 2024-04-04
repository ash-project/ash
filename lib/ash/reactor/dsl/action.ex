defmodule Ash.Reactor.Dsl.Action do
  @moduledoc """
  The `action` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            async?: true,
            authorize?: nil,
            description: nil,
            domain: nil,
            inputs: [],
            name: nil,
            resource: nil,
            tenant: [],
            transform: nil,
            type: :action,
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
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          name: atom,
          resource: module,
          tenant: [Ash.Reactor.Dsl.Tenant.t()],
          type: :action,
          undo_action: atom,
          undo: :always | :never | :outside_transaction,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :action,
      describe: """
      Declares a step that will call a generic action on a resource.

      #{Ash.Reactor.Dsl.Action.__shared_undo_docs__()}
      """,
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
      schema: __shared_action_option_schema__()
    }

  @doc false
  def __shared_action_option_schema__(include_undo? \\ true) do
    [
      action: [
        type: :atom,
        required: false,
        doc: """
        The name of the action to call on the resource.
        """
      ],
      domain: [
        type: {:spark, Ash.Domain},
        required: false,
        doc:
          "The Domain to use when calling the action.  Defaults to the Domain set on the resource or in the `ash` section."
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
      name: [
        type: :atom,
        required: true,
        doc: "A unique name for the step."
      ],
      resource: [
        type: {:spark, Ash.Resource},
        required: true,
        doc: """
        The resource to call the action on.
        """
      ]
    ]
    |> maybe_concat(
      [
        undo_action: [
          type: :atom,
          required: false,
          doc: """
          The name of the action to call on the resource when the step is to be undone.
          """
        ],
        undo: [
          type: {:in, [:always, :never, :outside_transaction]},
          required: false,
          default: :never,
          doc: "How to handle undoing this action"
        ]
      ],
      include_undo?
    )
  end

  def __shared_undo_docs__ do
    """
    > #### Undo behaviour {: .tip}
    >
    > This step has three different modes of undo.
    >
    > * `never` - The result of the action is never undone.  This is the default.
    > * `always` - The `undo_action` will always be called.
    > * `outside_transaction` - The `undo_action` will not be called when running inside a `transaction` block, but will be otherwise.
    """
  end

  defp maybe_concat(left, right, true), do: Enum.concat(left, right)
  defp maybe_concat(left, _right, _falsy), do: left
end
