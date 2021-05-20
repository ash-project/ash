defmodule Ash.Calculation do
  @moduledoc "The behaviour for a calculation module"
  defmacro __using__(opts) do
    type =
      opts[:type] ||
        raise "Must provide `type` option to `use Ash.Calculation` in #{__CALLER__.module}"

    unless Ash.Type.ash_type?(Ash.Type.get_type(opts[:type])) do
      raise "Value provided for `type` option must be a valid Ash type"
    end

    quote do
      @behaviour Ash.Calculation

      def init(opts), do: {:ok, opts}

      def type, do: unquote(type)

      def describe(opts), do: inspect({__MODULE__, opts})

      def select(_query, _opts), do: []

      defoverridable init: 1, type: 0, describe: 1, select: 2
    end
  end

  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback type() :: Ash.Type.t()
  @callback describe(Keyword.t()) :: String.t()
  @callback calculate([Ash.Resource.record()], Keyword.t(), map) ::
              {:ok, [term]} | [term] | {:error, term}
  @callback select(Ash.Query.t(), Keyword.t()) :: list(atom)
end
