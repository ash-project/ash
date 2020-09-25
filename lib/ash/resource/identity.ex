defmodule Ash.Resource.Identity do
  @moduledoc "Represents a unique constraint on a resource"
  defstruct [:name, :keys, :description]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc:
        "The name of the identity. Used by extensions to target specific identities for fetching single instances of a resource"
    ],
    keys: [
      type: {:custom, __MODULE__, :keys, []},
      required: true,
      doc:
        "The names of attributes, aggregates or calculations that uniquely identify this resource."
    ],
    description: [
      type: :string,
      doc: "An optional description for the identity"
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
