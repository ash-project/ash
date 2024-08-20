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

  @callback load_relationships(
              query :: Ash.Query.t(),
              results :: list(Ash.Resource.record()),
              opts :: Keyword.t(),
              context :: context(),
              lazy? :: boolean()
            ) ::
              {:ok, list(Ash.Resource.record())} | {:error, term}

  @callback read(
              query :: Ash.Query.t(),
              data_layer_query :: term,
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, list(Ash.Resource.record())} | {:error, term}

  @optional_callbacks [
    load_relationships: 5
  ]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRead
      @before_compile Ash.Resource.ManualRead
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:load_relationships, 5}, :def) do
        def has_load_relationships?, do: true
      else
        def has_load_relationships?, do: false
      end
    end
  end
end
