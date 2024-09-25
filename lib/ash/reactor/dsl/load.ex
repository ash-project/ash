defmodule Ash.Reactor.Dsl.Load do
  @moduledoc """
  The `load` step entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: false,
            action: nil,
            actor: nil,
            async?: true,
            authorize?: nil,
            context: nil,
            description: nil,
            domain: nil,
            lazy?: nil,
            load: nil,
            name: nil,
            records: nil,
            reuse_values?: nil,
            strict?: nil,
            tenant: nil,
            transform: nil,
            type: :load,
            wait_for: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action: nil | atom,
          action_step?: false,
          actor: nil | Ash.Reactor.Dsl.Actor.t(),
          async?: boolean,
          authorize?: nil | boolean,
          context: nil | Ash.Reactor.Dsl.Context.t(),
          description: nil | String.t(),
          domain: nil | Ash.Domain.t(),
          lazy?: nil | boolean,
          load: Reactor.Template.t(),
          name: atom,
          records: Reactor.Template.t(),
          reuse_values?: nil | boolean,
          strict?: nil | boolean,
          tenant: nil | Ash.Reactor.Dsl.Tenant.t(),
          transform: nil | (any -> any) | {module, keyword} | mfa,
          type: :load,
          wait_for: [Reactor.Dsl.WaitFor.t()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :load,
      describe: "Declares a step that will load additional data on a resource.",
      target: __MODULE__,
      args: [:name, :records, :load],
      imports: [Reactor.Dsl.Argument],
      entities: [
        actor: [Ash.Reactor.Dsl.Actor.__entity__()],
        context: [Ash.Reactor.Dsl.Context.__entity__()],
        tenant: [Ash.Reactor.Dsl.Tenant.__entity__()],
        wait_for: [Reactor.Dsl.WaitFor.__entity__()]
      ],
      singleton_entity_keys: [:actor, :context, :tenant],
      recursive_as: :steps,
      schema:
        false
        |> Ash.Reactor.Dsl.Action.__shared_action_option_schema__()
        |> Keyword.take([:domain, :async?, :authorize?, :description, :name])
        |> Keyword.merge(
          records: [
            type: Reactor.Template.type(),
            required: true,
            doc: "The records upon which to add extra loaded data"
          ],
          transform: [
            type:
              {:or, [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.Transform, 1}}, nil]},
            required: false,
            default: nil,
            doc:
              "An optional transformation function which can be used to modify the load statement before it is passed to the load."
          ],
          load: [
            type: Reactor.Template.type(),
            required: true,
            doc: "An Ash load statement"
          ],
          lazy?: [
            type: :boolean,
            required: false,
            doc:
              "If set to true, values will only be loaded if the related value isn't currently loaded."
          ],
          reuse_values?: [
            type: :boolean,
            required: false,
            doc:
              "Whether calculations are allowed to reuse values that have already been loaded, or must refetch them from the data layer."
          ],
          strict?: [
            type: :boolean,
            required: false,
            doc:
              "If set to true, only specified attributes will be loaded when passing a list of fields to fetch on a relationship, which allows for more optimized data-fetching."
          ]
        )
    }
end
