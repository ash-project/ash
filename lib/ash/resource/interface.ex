defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface
  """
  defstruct [:name, :action, :args, :params, :get?]

  @type t :: %__MODULE__{}

  def interface_options do
    [
      tenant: [
        type: :any,
        doc: "set the tenant of the query/changeset"
      ],
      context: [
        type: :any,
        doc: "set context on the query/changeset"
      ],
      actor: [
        type: :any,
        doc: "set the actor for authorization"
      ],
      authorize?: [
        type: :boolean,
        doc: """
        whether or not to perform authorization.
        If an actor option is provided (even if it is `nil`), defaults to `true`. If not, defaults to `false`.
        """
      ],
      verbose?: [
        type: :boolean,
        doc: "a flag to toggle verbose output from the internal Ash engine (for debugging)"
      ]
    ]
  end

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
      type: {:list, :atom},
      doc: """
      Map specific arguments to named inputs. Can provide any argument/attributes that the action allows.
      """
    ],
    params: [
      type: {:in, [:none, :optional, :required]},
      doc: """
      * `:none`: The generated function doesn't accept a map of params
      * `:optional`: The generated function accepts a map of params but doesn't require it
      * `:required`: The function will be generated in a way that requires a map of params
      """
    ],
    get?: [
      type: :boolean,
      doc: """
      Only relevant for read actions. Expects to only receive a single result from a read action.

      For example, `get_user_by_email`.
      """
    ]
  ]

  def schema, do: @schema
end
