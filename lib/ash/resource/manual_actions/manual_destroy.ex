defmodule Ash.Resource.ManualDestroy do
  @moduledoc """
  A module to implement manual destroy actions.
  """

  @type context :: %{
          actor: term,
          tenant: term,
          authorize?: term,
          api: module
        }

  @callback destroy(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
