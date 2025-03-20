defmodule Ash.Reactor.Dsl.Get do
  @moduledoc """
  The `get` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: [],
            async?: true,
            authorize?: nil,
            by: [],
            context: nil,
            description: nil,
            domain: nil,
            guards: [],
            fail_on_not_found?: nil,
            inputs: [],
            load: nil,
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
          actor: nil | Ash.Reactor.Dsl.Actor.t(),
          async?: boolean,
          authorize?: boolean | nil,
          by: [atom],
          context: nil | Ash.Reactor.Dsl.Context.t(),
          description: String.t() | nil,
          domain: Ash.Domain.t(),
          guards: [Reactor.Guard.Build.t()],
          fail_on_not_found?: boolean,
          inputs: [Ash.Reactor.Dsl.Inputs.t()],
          load: nil | Ash.Reactor.Dsl.ActionLoad.t(),
          name: atom,
          resource: module,
          tenant: nil | Ash.Reactor.Dsl.Tenant.t(),
          type: :create,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :get,
      describe:
        "Declares a step that will call a read action on a resource returning a single record.",
      examples: [
        """
        get :post, MyApp.Post, :read do
          by [:id]
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
          by: [
            type: {:wrap_list, :atom},
            required: true,
            doc: "The fields to filter by"
          ],
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
