defmodule Ash.Resource.ManualUpdate do
  @moduledoc """
  A module to implement manual update actions.
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

  @callback update(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @callback bulk_update(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              list(
                :ok
                | {:ok, Ash.Resource.record()}
                | {:error, Ash.Error.t()}
                | {:notifications, list(Ash.Notifier.Notification.t())}
              )

  @optional_callbacks [bulk_update: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualUpdate
    end
  end
end
