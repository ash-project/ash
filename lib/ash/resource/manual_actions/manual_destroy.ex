defmodule Ash.Resource.ManualDestroy do
  @moduledoc """
  A module to implement manual destroy actions.
  """

  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:api) => module,
          optional(any) => any
        }

  @callback destroy(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
