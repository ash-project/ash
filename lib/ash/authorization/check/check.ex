defmodule Ash.Authorization.Check do
  @moduledoc """
  A behaviour for declaring checks, which can be used to easily construct
  authorization rules.
  """

  @type options :: Keyword.t()

  @callback strict_check(Ash.user(), Ash.Authorization.request(), options) ::
              list(Ash.Authorization.precheck_result())
  @callback check(Ash.user(), Ash.Authorization.request(), options) :: boolean
  @callback describe(options()) :: String.t()

  @optional_callbacks check: 3

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Authorization.Check
    end
  end
end
