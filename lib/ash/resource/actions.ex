defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
    end
  end

  defmacro get(boolean) do
    quote do
      @actions Keyword.put(@actions, :get, unquote(boolean))
    end
  end
end
