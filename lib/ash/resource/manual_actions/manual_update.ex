defmodule Ash.Resource.ManualUpdate do
  @moduledoc """
  A module to implement manual update actions.
  """

  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:api) => module,
          optional(any) => any
        }

  @callback update(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @callback bulk_update(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              list(
                {:ok, Ash.Resource.record()}
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
