# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Aggregate.CustomAggregate do
  @moduledoc """
  The root behavior for a custom aggregate.

  See data layers for their implementation of custom aggregates.
  """
  @typedoc """
  A custom aggregate implementation module and its configured options.
  """
  @type t :: {module(), Keyword.t()}
  @doc """
  Returns a string describing the custom aggregate and its configuration.

  The default is the inspected `{module, opts}` tuple.
  """
  @callback describe(t()) :: String.t()

  @doc """
  Sets up a custom aggregate module and provides a default `c:describe/1`.

  Implement the aggregate operations required by the data layer that will
  execute it. See [Aggregates](/documentation/topics/resources/aggregates.md).
  """
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
