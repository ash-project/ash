defmodule Ash.Resource.ManualRead do
  @moduledoc """
  A module to implement manual read actions.
  """

  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:domain) => module,
          optional(any) => any
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
