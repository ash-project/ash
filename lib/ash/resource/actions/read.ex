defmodule Ash.Resource.Actions.Read do
  @moduledoc "Represents a read action on a resource."

  defstruct [:name, :pagination, :primary?, :filter, :description, type: :read]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  filter: [
                    type: :any,
                    doc:
                      "A filter template, that may contain actor references. See `Ash.Filter` for more on templates"
                  ],
                  pagination: [
                    type: {:custom, __MODULE__, :pagination, []},
                    doc:
                      "Options for how the action should support pagination. See the pagination section for more information.",
                    default: false
                  ]
                ],
                @global_opts,
                "Action Options"
              )

  @pagination_schema [
    keyset?: [
      type: :boolean,
      doc: "Whether or not keyset based pagination is supported",
      default: false
    ],
    offset?: [
      type: :boolean,
      doc: "Whether or not offset based pagination is supported",
      default: false
    ],
    default_limit: [
      type: :pos_integer,
      doc: "The default page size to apply, if one is not supplied"
    ],
    countable: [
      type: {:one_of, [true, false, :by_default]},
      doc:
        "Whether not a returned page will have a full count of all records. Use `:by_default` to do it automatically.",
      default: false
    ],
    max_page_size: [
      type: :pos_integer,
      doc: "The maximum amount of records that can be requested in a single page",
      default: 250
    ],
    required?: [
      type: :boolean,
      doc:
        "Whether or not pagination can be disabled. Only relevant if some pagination configuration is supplied.",
      default: true
    ]
  ]

  defmodule Pagination do
    @moduledoc "Represents the pagination configuration of a read action"
    defstruct [
      :default_limit,
      :max_page_size,
      countable: false,
      required?: false,
      keyset?: false,
      offset?: false
    ]
  end

  def pagination(false) do
    {:ok, %Pagination{}}
  end

  def pagination(opts) do
    case NimbleOptions.validate(opts, @pagination_schema) do
      {:ok, result} ->
        pagination = struct(Pagination, result)

        if pagination.keyset? or pagination.offset? do
          {:ok, pagination}
        else
          {:error, "Must enable `keyset?` or `offset?`"}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def opt_schema, do: @opt_schema
  def pagination_schema, do: @pagination_schema
end
