defmodule Ash.Calculation do
  @moduledoc """
  The behaviour for a calculation module

  Use `select/2` to apply a select statement when the calculation is loaded.
  This does not apply in the case that you are loading on existing resources using
  `MyApi.load`. It also doesn't apply when the calculation is used in a filter or sort,
  because it is not necessary to select fields to power filters done in the data layer.
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Calculation

      def init(opts), do: {:ok, opts}

      def describe(opts), do: "##{inspect(__MODULE__)}<#{inspect(opts)}>"

      def load(query, _opts, _context), do: query

      def select(_query, _opts, _context), do: []

      defoverridable init: 1, describe: 1, select: 3, load: 3
    end
  end

  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback describe(Keyword.t()) :: String.t()
  @callback calculate([Ash.Resource.record()], Keyword.t(), map) ::
              {:ok, [term]} | [term] | {:error, term} | :unknown
  @callback expression(Keyword.t(), map) :: any
  @callback load(Ash.Query.t(), Keyword.t(), map) :: Ash.Query.t()
  @callback select(Ash.Query.t(), Keyword.t(), map) :: list(atom)

  @optional_callbacks expression: 2, calculate: 3
end
