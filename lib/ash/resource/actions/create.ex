defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    manual?: false,
    manual: nil,
    touches_resources: [],
    require_attributes: [],
    arguments: [],
    changes: [],
    allow_nil_input: [],
    reject: [],
    metadata: [],
    transaction?: true,
    type: :create
  ]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          accept: list(atom),
          manual: module | nil,
          allow_nil_input: list(atom),
          touches_resources: list(atom),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          primary?: boolean,
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                allow_nil_input: [
                  type: {:list, :atom},
                  doc: """
                  A list of attributes that would normally be required, but should not be for this action. They will still be validated just before
                  the record is created.
                  """,
                  links: [
                    guides: [
                      "ash:guide:Actions"
                    ]
                  ]
                ],
                manual: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.ManualCreate,
                     {Ash.Resource.ManualCreate.Function, 2}},
                  links: [
                    guides: [
                      "ash:guide:Manual Actions"
                    ]
                  ],
                  doc: """
                  Override the creation behavior. See the manual action guides for more.
                  """
                ]
              ]
              |> Spark.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )
              |> Spark.OptionsHelpers.merge_schemas(
                @create_update_opts,
                "Create/Update Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
