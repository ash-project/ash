# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.NotAtomicOverRange do
  @moduledoc "Used when a bulk write over a range cannot be performed atomically"

  use Splode.Error, fields: [:resource, :action, :as_of], class: :framework

  def message(error) do
    """
    Cannot run #{inspect(error.resource)}.#{error.action} over #{inspect(error.as_of)} without writing atomically.

    A write over a range spans every version it overlaps, and only an atomic write updates each
    of them from its own values. Use the `:atomic` or `:atomic_batches` strategy, and an action
    that can be performed atomically.
    """
  end
end
