defmodule Ash.Authorization.Check do
  @moduledoc """
  A behaviour for declaring checks, which can be used to easily construct
  authorization rules.
  """

  @type options :: Keyword.t()

  @callback strict_check(Ash.user(), Ash.Authorization.request(), options) :: boolean | :unknown

  @callback prepare(Ash.user(), Ash.Authorization.request(), options) ::
              list(Ash.Authorization.prepare_instruction()) | {:error, Ash.error()}
  @callback check(Ash.user(), list(Ash.record()), Ash.Authorization.request(), options) ::
              {:ok, boolean | list(Ash.record())} | {:error, Ash.error()}
  @callback describe(options()) :: String.t()

  @optional_callbacks check: 4, prepare: 3

  def defines_check?(module) do
    :erlang.function_exported(module, :check, 4)
  end

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Authorization.Check

      @impl true
      def prepare(_, _, _), do: []

      defoverridable prepare: 3
    end
  end
end
