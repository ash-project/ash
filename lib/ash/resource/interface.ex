defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface
  """
  defstruct [:name, :action, :args, :get?, :get_by, :get_by_identity, :not_found_error?]

  @type t :: %__MODULE__{}

  def transform(definition) do
    if definition.get_by || definition.get_by_identity do
      {:ok, %{definition | get?: true}}
    else
      {:ok, definition}
    end
  end

  def interface_options(:calculate) do
    [
      actor: [
        type: :any,
        doc: "The actor to use for actor references"
      ]
    ]
  end

  def interface_options(:create) do
    opts = Ash.create_opts()

    Keyword.merge(opts,
      changeset: [
        type: :any,
        doc: "A changeset to seed the action with."
      ],
      bulk_options: [
        type: :keyword_list,
        doc: "Options passed to `Ash.bulk_create`, if a list or stream of inputs is provided.",
        keys: Keyword.drop(Ash.bulk_create_opts(), Keyword.keys(opts))
      ]
    )
  end

  def interface_options(:update) do
    opts = Ash.update_opts()

    Keyword.merge(opts,
      bulk_options: [
        type: :keyword_list,
        doc:
          "Options passed to `Ash.bulk_create`, if a query, list, or stream of inputs is provided.",
        keys: Keyword.drop(Ash.bulk_update_opts(), Keyword.keys(opts) ++ [:resource])
      ]
    )
  end

  def interface_options(:destroy) do
    opts = Ash.destroy_opts()

    Keyword.merge(opts,
      bulk_options: [
        type: :keyword_list,
        doc:
          "Options passed to `Ash.bulk_create`, if a query, list, or stream of inputs is provided.",
        keys: Keyword.drop(Ash.bulk_destroy_opts(), Keyword.keys(opts) ++ [:resource])
      ]
    )
  end

  def interface_options(:read) do
    opts = Ash.read_opts()

    Keyword.merge(opts,
      query: [
        type: :any,
        doc: "A query to seed the action with."
      ],
      stream?: [
        type: :boolean,
        default: false,
        doc: "If true, a stream of the results will be returned"
      ],
      not_found_error?: [
        type: :boolean,
        doc:
          "Whether or not to return or raise a `NotFound` error or to return `nil` when a get? action/interface is called."
      ],
      stream_options: [
        type: :keyword_list,
        doc: "Options passed to `Ash.stream!`, if `stream?: true` is given",
        keys: Keyword.drop(Ash.stream_opts(), Keyword.keys(opts))
      ]
    )
  end

  def interface_options(_), do: []

  @schema [
    name: [
      type: :atom,
      doc: "The name of the function that will be defined",
      required: true
    ],
    action: [
      type: :atom,
      doc:
        "The name of the action that will be called. Defaults to the same name as the function."
    ],
    args: [
      type: {:list, {:or, [:atom, {:tagged_tuple, :optional, :atom}]}},
      doc: """
      Map specific arguments to named inputs. Can provide any argument/attributes that the action allows.
      """
    ],
    not_found_error?: [
      type: :boolean,
      default: true,
      doc:
        "If the action or interface is configured with `get?: true`, this determines whether or not an error is raised or `nil` is returned."
    ],
    get?: [
      type: :boolean,
      doc: """
      Expects to only receive a single result from a read action, and returns a single result instead of a list. Ignored for other action types.
      """
    ],
    get_by: [
      type: {:wrap_list, :atom},
      doc: """
      Takes a list of fields and adds those fields as arguments, which will then be used to filter. Sets `get?` to true automatically. Ignored for non-read actions.
      """
    ],
    get_by_identity: [
      type: :atom,
      doc: """
      Only relevant for read actions. Takes an identity, and gets its field list, performing the same logic as `get_by` once it has the list of fields.
      """
    ]
  ]

  def schema, do: @schema
end
