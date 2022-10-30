defmodule Ash.Resource.ManualCreate do
  @moduledoc """
  A module to implement manual create actions.
  """

  @type context :: %{
          actor: term,
          tenant: term,
          authorize?: term,
          api: module
        }

  @callback create(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, Ash.Resource.record()} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualCreate
    end
  end
end
