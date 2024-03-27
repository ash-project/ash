defmodule Ash.Resource.ManualRelationship do
  @moduledoc """
  A module to implement manual relationships.
  """

  defmodule Context do
    @moduledoc "The context passed into manual relationship functions"
    defstruct [
      :relationship,
      :query,
      :actor,
      :tenant,
      :authorize?,
      :domain
    ]

    @type t :: %__MODULE__{
            relationship: Ash.Resource.Relationships.relationship(),
            query: Ash.Query.t(),
            actor: term,
            tenant: term,
            authorize?: boolean,
            domain: module
          }
  end

  @callback select(opts :: Keyword.t()) :: list(atom)

  @callback load(
              list(Ash.Resource.record()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, map} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualRelationship

      def select(_opts), do: []

      defoverridable select: 1
    end
  end
end
