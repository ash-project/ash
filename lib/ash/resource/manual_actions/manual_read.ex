defmodule Ash.Resource.ManualRead do
  @moduledoc """
  A module to implement manual read actions.
  """

  @type context :: %{
          actor: term,
          tenant: term,
          authorize?: term,
          api: module
        }

  @callback read(
              query :: Ash.Query.t(),
              data_layer_query :: term,
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, list(Ash.Resource.record())} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRead
    end
  end
end
