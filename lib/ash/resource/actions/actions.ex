defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
    end
  end

  defmacro defaults(:all) do
    quote do
      defaults([:create, :update, :destroy, :read])
    end
  end

  defmacro defaults(defaults) do
    quote do
      for default <- unquote(defaults) do
        case default do
          :create ->
            create(:default, [])

          :update ->
            update(:default, [])

          :destroy ->
            destroy(:default, [])

          :read ->
            read(:default, [])

          action ->
            raise "Invalid action type #{action} listed in defaults list for resource: #{
                    __MODULE__
                  }"
        end
      end
    end
  end

  defmacro create(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action = Ash.Resource.Actions.Create.new(name, primary?: opts[:primary?] || false)

      @actions action
    end
  end

  defmacro update(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action = Ash.Resource.Actions.Update.new(name, primary?: opts[:primary?] || false)

      @actions action
    end
  end

  defmacro destroy(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action = Ash.Resource.Actions.Destroy.new(name, primary?: opts[:primary?] || false)

      @actions action
    end
  end

  defmacro read(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action = Ash.Resource.Actions.Read.new(name, primary?: opts[:primary?] || false)

      @actions action
    end
  end
end
