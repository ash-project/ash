defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
    end
  end

  defmacro get(opts) do
    quote do
      name = unquote(opts[:name]) || :get
      # TODO: do this somewhere centrally somewhere else
      path = unquote(opts[:path]) || Path.join("#{@name}/", unquote(opts[:path]) || "/:id")
      expose? = unquote(opts[:expose?]) || false
      @actions Ash.Resource.Actions.Action.new(name, :get, expose?: expose?, path: path)
    end
  end

  defmacro index(opts) do
    quote do
      name = unquote(opts[:name]) || :index
      path = "#{@name}/"
      expose? = unquote(opts[:expose?]) || false
      @actions Ash.Resource.Actions.Action.new(name, :index, expose?: expose?, path: path)
    end
  end
end
