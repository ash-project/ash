defmodule Ash.Resource.ManualUpdate do
  @moduledoc """
  A module to implement manual update actions.
  """

  @type context :: %{
          actor: term,
          tenant: term,
          authorize?: term,
          api: module
        }

  @callback update(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualUpdate
    end
  end
end
