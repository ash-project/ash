defmodule Ash.Reactor.Dsl.Read do
  @moduledoc """
  The `read` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: true,
            action: nil,
            actor: nil,
            context: nil,
            domain: nil,
            async?: true,
            authorize?: nil,
            description: nil,
            inputs: [],
            load: nil,
            name: nil,
            resource: nil,
            tenant: nil,
            transform: nil,
            type: :read,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action_step?: true,
          action: atom,
          actor: nil | Ash.Reactor.Dsl.Actor.t(),
          context: nil | Ash.Reactor.Dsl.Context.t(),
          domain: Ash.Domain.t(),
          async?: boolean,
          authorize?: boolean | nil,
          description: String.t() | nil,
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
      name: :read,
      describe: "Declares a step that will call a read action on a resource.",
      examples: [
        """
        read :read_posts, MyApp.Post, :read
        """,
        """
        read :read_posts_in_range, MyApp.Post, :read_in_range do
          inputs %{min_date: input(:min_date), max_date: input(:max_date)}
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
        inputs: [Ash.Reactor.Dsl.Inputs.__entity__()],
        load: [Ash.Reactor.Dsl.ActionLoad.__entity__()],
        tenant: [Ash.Reactor.Dsl.Tenant.__entity__()],
        wait_for: [Reactor.Dsl.WaitFor.__entity__()]
      ],
      singleton_entity_keys: [:actor, :context, :load, :tenant],
      recursive_as: :steps,
      schema: Ash.Reactor.Dsl.Action.__shared_action_option_schema__(false)
    }
end
