defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
    end
  end

  defmacro get(name \\ :get, _opts \\ []) do
    quote do
      action = Ash.Resource.Actions.Action.new(unquote(name), :get)

      @actions action

      @current_action action

      def action(unquote(name)) do
        @current_action
      end
    end
  end

  defmacro index(name \\ :index, _opts \\ []) do
    quote do
      action = Ash.Resource.Actions.Action.new(unquote(name), :index)

      @actions action

      @current_action action

      def action(unquote(name)) do
        @current_action
      end
    end
  end
end
