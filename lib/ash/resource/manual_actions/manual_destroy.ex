defmodule Ash.Resource.ManualDestroy do
  @moduledoc """
  A module to implement manual destroy actions.

  Note that in the returns of these functions you must return the destroyed record or records.
  """

  defmodule Context do
    defstruct [
      :actor,
      :tenant,
      :tracer,
      :authorize?,
      :domain,
      :return_records?,
      :batch_size
    ]
  end

  @callback destroy(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @callback bulk_destroy(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              list(
                {:ok, Ash.Resource.record()}
                | {:error, Ash.Error.t()}
                | {:notifications, list(Ash.Notifier.Notification.t())}
              )

  @optional_callbacks [bulk_destroy: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
