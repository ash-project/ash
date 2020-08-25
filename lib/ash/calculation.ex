defmodule Ash.Calculation do
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

      defoverridable init: 1, type: 0
    end
  end

  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, Ash.error()}
  @callback type() :: Ash.Type.t()
  @callback describe(Keyword.t()) :: String.t()
  @callback calculate([Ash.record()], Keyword.t(), map) :: {:ok, [term]} | {:error, Ash.error()}
end
