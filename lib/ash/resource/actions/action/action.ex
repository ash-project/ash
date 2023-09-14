defmodule Ash.Resource.Actions.Action do
  @moduledoc "Represents a custom action on a resource."

  defstruct [
    :name,
    :description,
    :returns,
    :run,
    constraints: [],
    touches_resources: [],
    arguments: [],
    transaction?: false,
    primary?: false,
    type: :action
  ]

  @type t :: %__MODULE__{
          type: :action,
          name: atom,
          description: String.t() | nil,
          arguments: [Ash.Resource.Actions.Argument.t()],
          touches_resources: [Ash.Resource.t()],
          constraints: Keyword.t(),
          run: {module, Keyword.t()},
          returns: Ash.Type.t(),
          primary?: boolean,
          transaction?: boolean
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @opt_schema [
                returns: [
                  type: Ash.OptionsHelpers.ash_type(),
                  doc: "The return type of the action. See `Ash.Type` for more."
                ],
                constraints: [
                  type: :keyword_list,
                  doc: """
                  Constraints for the return type. See the [constriants topic](/documentation/topics/constraints.md) for more.
                  """
                ],
                run: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.Actions.Implementation,
                     {Ash.Resource.Action.ImplementationFunction, 2}}
                ]
              ]
              |> Spark.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
