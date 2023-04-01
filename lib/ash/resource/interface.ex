defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface
  """
  defstruct [:name, :action, :args, :get?, :get_by, :get_by_identity, :not_found_error?]

  @type t :: %__MODULE__{}

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
      if define_for = Ash.Resource.Info.define_interface_for(__MODULE__) do
        require Ash.CodeInterface

        Ash.CodeInterface.define_interface(
          define_for,
          __MODULE__
        )
      end
    end
  end

  def transform(definition) do
    if definition.get_by || definition.get_by_identity do
      {:ok, %{definition | get?: true}}
    else
      {:ok, definition}
    end
  end

  def interface_options(action_type) do
    [
      tenant: [
        type: :any,
        doc: "Set the tenant of the query/changeset"
      ],
      context: [
        type: :any,
        doc: "Set context on the query/changeset"
      ],
      actor: [
        type: :any,
        doc: "set the actor for authorization"
      ],
      actor: [
        type: :any,
        doc: "set the tracer for the action"
      ],
      authorize?: [
        type: :boolean,
        doc: """
        Whether or not to perform authorization. The default behavior depends on the api configuration.
        """
      ],
      verbose?: [
        type: :boolean,
        doc: "a flag to toggle verbose output from the internal Ash engine (for debugging)"
      ],
      timeout: [
        type: :timeout,
        doc: "A timeout to apply to the operation."
      ]
    ] ++ action_type_opts(action_type)
  end

  defp action_type_opts(:create) do
    [
      changeset: [
        type: :any,
        doc: "A changeset to seed the action with."
      ]
    ]
  end

  defp action_type_opts(:read) do
    [
      query: [
        type: :any,
        doc: "A query to seed the action with."
      ],
      load: [
        type: :any,
        doc: "Adds a load statement to the query before passing it to the action."
      ],
      not_found_error?: [
        type: :boolean,
        doc:
          "Whether or not to return or raise a `NotFound` error or to return `nil` when a get? action/interface is called."
      ]
    ]
  end

  defp action_type_opts(_), do: []

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
      type: {:list, :atom},
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
