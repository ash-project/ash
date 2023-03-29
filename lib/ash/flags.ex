defmodule Ash.Flags do
  @moduledoc """
  Feature flagging support for Ash internals.

  These are macros so that they can be used at compile time to switch code
  paths.
  """

  @doc "Should read actions use the new flow-based executor?"
  @spec read_uses_flow? :: Macro.t()
  defmacro read_uses_flow? do
    quote do
      :ash
      |> Application.compile_env(:flags, [])
      |> Keyword.get(:read_uses_flow?, false)
    end
  end
end
