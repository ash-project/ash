# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Action do
  @moduledoc """
  Represents a resource action in the API specification.
  """

  @type action_type :: :read | :create | :update | :destroy | :action

  @type t :: %__MODULE__{
          name: atom(),
          type: action_type(),
          description: String.t() | nil,
          primary?: boolean(),
          get?: boolean(),
          arguments: [Ash.Info.Manifest.Argument.t()],
          accept: [atom()] | nil,
          require_attributes: [atom()] | nil,
          allow_nil_input: [atom()] | nil,
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
    :arguments,
    :accept,
    :require_attributes,
    :allow_nil_input,
    :metadata,
    :returns,
    :pagination,
    custom: %{}
  ]
end
