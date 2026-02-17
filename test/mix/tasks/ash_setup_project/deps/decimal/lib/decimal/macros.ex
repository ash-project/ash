defmodule Decimal.Macros do
  @moduledoc false

  defmacro doc_since(version) do
    if Version.match?(System.version(), ">= 1.7.0") do
      quote do
        @doc since: unquote(version)
      end
    end
  end
end
