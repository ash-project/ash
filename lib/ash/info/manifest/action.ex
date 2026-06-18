# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Action do
  @moduledoc """
  Represents a resource action in the API specification.

  `inputs` is the unified list of action inputs — action arguments and any
  attributes the action accepts are normalized into a single list of
  `%Ash.Info.Manifest.Argument{}` entries. Each entry's `allow_nil?` and
  `has_default?` reflect the action's effective semantics (e.g. an accepted
  attribute named in `require_attributes` will have `allow_nil?: false`; one
  named in `allow_nil_input` will have `allow_nil?: true`).
  """

  @type action_type :: :read | :create | :update | :destroy | :action

  @type t :: %__MODULE__{
          name: atom(),
          type: action_type(),
          description: String.t() | nil,
          primary?: boolean(),
          get?: boolean(),
          inputs: [Ash.Info.Manifest.Argument.t()],
          metadata: [Ash.Info.Manifest.Metadata.t()],
          returns: Ash.Info.Manifest.Type.t() | nil,
          pagination: Ash.Info.Manifest.Pagination.t() | nil,
          custom: map()
        }

  defstruct [
    :name,
    :type,
    :description,
    :primary?,
    :get?,
    :inputs,
    :metadata,
    :returns,
    :pagination,
    custom: %{}
  ]
end
