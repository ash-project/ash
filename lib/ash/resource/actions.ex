defmodule Ash.Resource.Actions do
  defmacro get(boolean) do
    quote do
      @actions Keyword.put(@actions, :get, unquote(boolean))
    end
  end
end
