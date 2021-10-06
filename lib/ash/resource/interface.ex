defmodule Ash.Resource.Interface do
  @moduledoc """
  Represents a function in a resource's code interface
  """
  defstruct [:name, :action, :args, :get?]

  @type t :: %__MODULE__{}

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
      if Ash.Resource.Info.define_interface_in_resource?(__MODULE__) do
        require Ash.CodeInterface

        Ash.CodeInterface.define_interface(
          Ash.Resource.Info.define_interface_for(__MODULE__),
          __MODULE__
        )
      end
    end
  end

  def interface_options(action_type) do
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
      type: {:list, :atom},
      doc: """
      Map specific arguments to named inputs. Can provide any argument/attributes that the action allows.
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
