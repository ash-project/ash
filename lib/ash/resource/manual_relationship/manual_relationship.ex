defmodule Ash.Resource.ManualRelationship do
  @moduledoc """
  A module to implement manual relationships.
  """

  @type context :: %{
          relationship: Ash.Resource.Relationships.relationship(),
          query: Ash.Query.t(),
          root_query: Ash.Query.t(),
          actor: term,
          tenant: term,
          authorize?: term,
          api: module
        }

  @callback load(
              list(Ash.Resource.record()),
              opts :: Keyword.t(),
              context :: context()
            ) ::
              {:ok, map} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRelationship
    end
  end
end
