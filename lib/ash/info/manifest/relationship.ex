# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Relationship do
  @moduledoc """
  Represents a resource relationship in the API specification.
  """

  @type relationship_type :: :has_many | :has_one | :belongs_to | :many_to_many

  @type t :: %__MODULE__{
          name: atom(),
          type: relationship_type(),
          cardinality: :one | :many,
          destination: atom(),
          allow_nil?: boolean(),
          description: String.t() | nil,
          filterable?: boolean(),
          sortable?: boolean()
        }

  defstruct [
    :name,
    :type,
    :cardinality,
    :destination,
    :allow_nil?,
    :description,
    :filterable?,
    :sortable?
  ]
end
