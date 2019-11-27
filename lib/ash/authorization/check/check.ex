defmodule Ash.Authorization.Check do
  @moduledoc """
  A behaviour for declaring checks, which can be used to easily construct
  authorization rules.
  """

  alias Ash.Authorization.Rule

  @type options :: Keyword.t()

  @callback init(options()) :: {:ok, options()} | {:error, String.t()}
  @callback check(Rule.user(), Rule.data(), Rule.context(), options()) :: Rule.resource_ids()
  @callback describe(options()) :: String.t()
  @callback precheck(Rule.user(), Rule.context(), options()) ::
              Rule.precheck_result() | list(Rule.precheck_result())

  @optional_callbacks precheck: 3

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Authorization.Check

      def init(opts), do: opts

      defoverridable init: 1
    end
  end
end
