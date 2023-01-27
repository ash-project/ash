defmodule Ash.Error.Stacktrace do
  @moduledoc "A placeholder for a stacktrace so that we can avoid printing it everywhere"
  defstruct [:stacktrace]

  defimpl Inspect do
    def inspect(_, _) do
      "#Stacktrace<>"
    end
  end
end
