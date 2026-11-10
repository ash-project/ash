# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Delivery.Inline do
  @moduledoc """
  The default delivery: runs each listener immediately, during the phase in which the signal is
  emitted.

  Each listener handles every signal emitted together before the next listener runs. During
  `:before_action` and `:after_action`, listeners run inside the emitting action's transaction, so
  if one fails, the emitting action fails and is rolled back.
  """
  @behaviour Ash.Signals.Delivery

  @impl true
  def deliver(signals, listeners, _opts, context) do
    opts = Ash.Scope.to_opts(context)

    Enum.reduce_while(listeners, :ok, fn listener, :ok ->
      case Ash.Signals.run_listener(listener, signals, opts) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end
end
