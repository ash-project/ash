defmodule Ash.Reactor.Dsl.Change do
  @moduledoc """
  The `change` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            action_step?: false,
            always_atomic?: false,
            arguments: [],
            async?: true,
            change: nil,
            description: nil,
            initial: nil,
            name: nil,
            only_when_valid?: false,
            type: :change,
            where: []

  @type t :: %__MODULE__{
          __identifier__: any,
          action_step?: false,
          always_atomic?: false,
          async?: boolean,
          arguments: [Reactor.Dsl.Argument.t()],
          change: Ash.Resource.Change.t() | Ash.Resource.Change.ref(),
          description: nil | String.t(),
          initial: nil | Reactor.Template.t(),
          name: atom(),
          type: :change,
          where: [Ash.Resource.Validation.ref()]
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :change,
      describe: "Declares a step that will modify a changeset.",
      target: __MODULE__,
      args: [:name, :change],
      imports: [
        Reactor.Dsl.Argument,
        Ash.Resource.Change.Builtins,
        Ash.Resource.Validation.Builtins
      ],
      no_depend_modules: [:change],
      entities: [
        arguments: [
          Reactor.Dsl.Argument.__entity__(),
          Reactor.Dsl.WaitFor.__entity__()
        ]
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: "A unique name for this step."
        ],
        initial: [
          type: {:or, [nil, {:spark, Ash.Resource}, Reactor.Template.type()]},
          required: false,
          doc: "The initial value to work from, either a resource or a changeset"
        ],
        description: [
          type: {:or, [:string, nil]},
          required: false,
          doc: "An optional description for the change"
        ],
        only_when_valid?: [
          type: :boolean,
          default: false,
          doc: """
          If the change should only be run on valid changes. By default, all changes are run unless stated otherwise here.
          """
        ],
        change: [
          type:
            {:spark_function_behaviour, Ash.Resource.Change, Ash.Resource.Change.Builtins,
             {Ash.Resource.Change.Function, 2}},
          doc: """
          The module and options for a change. Also accepts a function that takes the changeset and the context. See `Ash.Resource.Change.Builtins` for builtin changes.
          """,
          required: true
        ],
        where: [
          type:
            {:wrap_list,
             {:spark_function_behaviour, Ash.Resource.Validation,
              Ash.Resource.Validation.Builtins, {Ash.Resource.Validation.Function, 1}}},
          required: false,
          default: [],
          doc: """
          Validations that should pass in order for this change to apply. These validations failing will result in this change being ignored.
          """
        ],
        always_atomic?: [
          type: :boolean,
          default: false,
          doc:
            "By default, changes are only run atomically if all changes will be run atomically or if there is no `change/3` callback defined. Set this to `true` to run it atomically always."
        ]
      ]
    }
end
