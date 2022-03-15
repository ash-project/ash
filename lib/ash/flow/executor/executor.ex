defmodule Ash.Flow.Executor do
  @moduledoc "A flow executor runs a given flow module"
  @type built_flow :: any
  @callback build(Ash.Flow.t(), input :: map, opts :: Keyword.t()) ::
              {:ok, built_flow()} | {:error, term}
  @callback execute(built_flow(), input :: map, opts :: Keyword.t()) ::
              {:ok, term} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Flow.Executor

      def build(flow, _input, _opts), do: {:ok, flow}

      defoverridable build: 3
    end
  end
end
