defmodule Ash.Resource.Identity do
  @moduledoc """
  Represents a unique constraint on a resource

  Data layers should (and all built in ones do), discount `nil` or `null` (in the case of postgres) values
  when determining if a unique constraint matches. This often means that you should
  prefer to use identities with non-nullable columns.

  Eventually, features could be added to support including `nil` or `null` values, but they would
  need to include a corresponding feature for data layers.
  """
  defstruct [:name, :keys, :description, :message, :eager_check_with, :pre_check_with]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the identity.",
      links: []
    ],
    keys: [
      type: {:custom, __MODULE__, :keys, []},
      required: true,
      doc: "The names of the attributes that uniquely identify this resource.",
      links: []
    ],
    eager_check_with: [
      type: {:behaviour, Ash.Api},
      doc: """
      Validates that the unique identity provided is unique at validation time, outside of any transactions, using the api module provided.
      """,
      links: []
    ],
    pre_check_with: [
      type: {:behaviour, Ash.Api},
      doc: """
      Validates that the unique identity provided is unique in a before_action hook.
      """,
      links: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the identity",
      links: [
        guides: [
          "ash:guide:Documentation"
        ]
      ]
    ],
    message: [
      type: :string,
      doc: "An error message to use when the unique identity would be violated",
      links: [
        guides: [
          "ash:guide:Errors"
        ]
      ]
    ]
  ]

  def schema, do: @schema

  @type t :: %__MODULE__{
          name: atom(),
          keys: list(atom()),
          description: String.t() | nil
        }

  def keys(keys) do
    keys = List.wrap(keys)

    if Enum.all?(keys, &is_atom/1) do
      {:ok, keys}
    else
      {:error, "Expected a list of atoms for the identity keys"}
    end
  end
end
