defmodule Ash.Resource.Identity do
  @moduledoc """
  Represents a unique constraint on a resource

  Data layers should (and all built in ones do), discount `nil` or `null` (in the case of postgres) values
  when determining if a unique constraint matches. This often means that you should
  prefer to use identities with non-nullable columns.

  Eventually, features could be added to support including `nil` or `null` values, but they would
  need to include a corresponding feature for data layers.
  """
  defstruct [
    :name,
    :keys,
    :description,
    :message,
    :eager_check?,
    :eager_check_with,
    :where,
    :pre_check?,
    :pre_check_with,
    :all_tenants?,
    nils_distinct?: false
  ]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the identity."
    ],
    keys: [
      type: {:wrap_list, :atom},
      required: true,
      doc: "The names of the attributes that uniquely identify this resource."
    ],
    where: [
      type: :any,
      doc:
        "A filter that expresses only matching records are unique on the provided keys. Ignored on embedded resources."
    ],
    nils_distinct?: [
      type: :boolean,
      doc:
        "Whether or not `nil` values are considered always distinct from eachother. `nil` values won't conflict with eachother unless you set this option to `false`.",
      default: true
    ],
    eager_check?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this identity is validated to be unique at validation time."
    ],
    eager_check_with: [
      type: {:behaviour, Ash.Domain},
      doc: """
      Validates that the unique identity provided is unique at validation time, outside of any transactions, using the domain module provided. Will default to resource's domain.
      """
    ],
    pre_check?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this identity is validated to be unique in a before_action hook."
    ],
    pre_check_with: [
      type: {:behaviour, Ash.Domain},
      doc: """
      Validates that the unique identity provided is unique in a before_action hook.
      """
    ],
    description: [
      type: :string,
      doc: "An optional description for the identity"
    ],
    message: [
      type: :string,
      doc: "An error message to use when the unique identity would be violated"
    ],
    all_tenants?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not this identity is unique across all tenants. If the resource is not multitenant, has no effect."
    ]
  ]

  def schema, do: @schema

  @type t :: %__MODULE__{
          name: atom(),
          keys: list(atom()),
          description: String.t() | nil,
          where: nil | Ash.Expr.t(),
          nils_distinct?: boolean(),
          all_tenants?: boolean()
        }
end
