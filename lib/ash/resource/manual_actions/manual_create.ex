defmodule Ash.Resource.ManualCreate do
  @moduledoc """
  A module to implement manual create actions.
  """

  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:tracer) => term,
          optional(:authorize?) => boolean,
          optional(:domain) => module,
          optional(any) => any
        }

  @callback create(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @callback bulk_create(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              list(
                {:ok, Ash.Resource.record()}
                | {:error, Ash.Error.t()}
                | {:notifications, list(Ash.Notifier.Notification.t())}
              )

  @optional_callbacks [bulk_create: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualCreate
    end
  end
end
