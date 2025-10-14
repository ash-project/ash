# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.MustBeAtomic do
  @moduledoc "Used when an action that must be atomic cannot be done atomically"

  use Splode.Error, fields: [:resource, :action, :reason], class: :framework

  def message(error) do
    """
    #{inspect(error.resource)}.#{error.action} must be performed atomically, but it could not be

    Reason: #{error.reason}

    See https://hexdocs.pm/ash/update-actions.html#fully-atomic-updates for more on atomics.
    """
  end
end
