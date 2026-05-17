# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Entrypoint do
  @moduledoc """
  Represents an action entrypoint in the API specification.

  Each entrypoint pairs a resource module with an action definition,
  representing an operation that clients can invoke.

  The `config` map carries extension-specific metadata, namespaced by
  extension key (e.g., `config.ash_typescript`). The generator passes
  this through opaquely — only the extension that populated it reads it.
  """

  @type t :: %__MODULE__{
          resource: atom(),
          action: Ash.Info.Manifest.Action.t(),
          config: map(),
          custom: map()
        }

  defstruct [:resource, :action, config: %{}, custom: %{}]
end
