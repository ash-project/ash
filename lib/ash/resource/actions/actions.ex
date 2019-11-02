defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
    end
  end

  # TODO: Originally I had it in my mind that you had to set up your own actions
  # for the basic capabilities. Instead, these capabilities will just automatically exist
  # for all resources. What you can do is create actions that are a variation of one of the
  # basic kinds of resource actions, with special rules. That will be hooked up later.

  # defmacro create(name \\ :create, opts \\ []) do
  #   quote bind_quoted: [name: name, opts: opts] do
  #     action = Ash.Resource.Actions.Create.new(name, primary?: opts[:primary?] || false)

  #     @actions action
  #   end
  # end

  # defmacro update(name \\ :update, opts \\ []) do
  #   quote bind_quoted: [name: name, opts: opts] do
  #     action = Ash.Resource.Actions.Update.new(name, primary?: opts[:primary?] || false)

  #     @actions action
  #   end
  # end

  # defmacro delete(name \\ :delete, opts \\ []) do
  #   quote bind_quoted: [name: name, opts: opts] do
  #     action = Ash.Resource.Actions.Delete.new(name, primary?: opts[:primary?] || false)

  #     @actions action
  #   end
  # end

  # defmacro get(name \\ :get, opts \\ []) do
  #   quote bind_quoted: [name: name, opts: opts] do
  #     action = Ash.Resource.Actions.Get.new(name, primary?: opts[:primary?] || false)

  #     @actions action
  #   end
  # end

  # defmacro index(name \\ :index, opts \\ []) do
  #   quote bind_quoted: [name: name, opts: opts] do
  #     action = Ash.Resource.Actions.Index.new(name, primary?: opts[:primary?] || false)

  #     @actions action
  #   end
  # end
end
