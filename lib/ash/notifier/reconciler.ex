defmodule Ash.Notifier.Reconciler do
  @moduledoc """
  This module is a shim for future functionality.

  Here is how it will work:
  This module takes the result of a query, the query, an `Ash.Notifier.Notification`,
  and some configuration, and reconciles the event with that state. This will be usable
  for things like cached data, phoenix liveviews, and keeping genserver state up to date.


  Right now, they simply return `:refetch`, which will be the instruction to rerun the query.
  If this module ever isn't sure how to handle a notification, it will simply return `:refetch`

  Over time, we can add smarter and smarter reconciliation, to reduce the cases where the
  data must be refetched.
  """
  defstruct [:data, state: :noop]

  def reconcile(data, _query, _notification, _config) when not is_list(data) do
    :refetch
  end

  def reconcile(_data, _query, _notification, _config) do
    # %__MODULE__{data: data}
    # |> reconcile_many(query, notification, config)
    # |> update_calculations(query, notification, config)
    # |> update_aggregates(query, notification, config)

    :refetch
  end
end
