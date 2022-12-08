defmodule Ash.Resource.Aggregate.CustomAggregate do
  @moduledoc """
  The root behavior for a custom aggregate.

  See data layers for their implementation of custom aggregates.
  """
  @type t :: {module(), Keyword.t()}
  @callback describe(t()) :: String.t()

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Aggregate.CustomAggregate

      def describe({module, opts}) do
        inspect({module, opts})
      end

      defoverridable describe: 1
    end
  end
end
