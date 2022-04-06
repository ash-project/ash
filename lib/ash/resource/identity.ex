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
      doc:
        "The name of the identity. Used by extensions to target specific identities for fetching single instances of a resource"
    ],
    keys: [
      type: {:custom, __MODULE__, :keys, []},
      required: true,
      doc:
        "The names of attributes, aggregates or calculations that uniquely identify this resource."
    ],
    eager_check_with: [
      type: {:behaviour, Ash.Api},
      doc: """
      Validates that the unique identity provided is unique at validation time, using the api module provided.

      The identity is checked on each validation of the changeset. For example, if you are using
      `AshPhoenix.Form`, this looks for a conflicting record on each call to `Form.validate/2`.
      For updates, it is only checked if one of the involved fields is being changed.

      For creates, The identity is checked unless your are performing an `upsert`, and the
      `upsert_identity` is this identity. Keep in mind that for this to work properly, you will need
      to pass the `upsert?: true, upsert_identity: :identity_name` *when creating the changeset* instead of
      passing it to the Api when creating.

      The `primary?` action is used to search for a record. This will error if you have not
      configured one.
      """
    ],
    pre_check_with: [
      type: {:behaviour, Ash.Api},
      doc: """
      Validates that the unique identity provided is unique *just prior* to enacting the resource action, using the Api provided.

      Behaves the same as `eager_check?`, but it runs just prior to the action being committed. Useful for
      data layers that don't support transactions/unique constraints, or manual resources with identities.
      """
    ],
    description: [
      type: :string,
      doc: "An optional description for the identity"
    ],
    message: [
      type: :string,
      doc: "An error message to use when the unique identity would be violated"
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
