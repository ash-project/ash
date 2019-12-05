defmodule Ash.Resource.Actions do
  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions

      import Ash.Authorization.Rule,
        only: [
          allow: 1,
          allow: 2,
          allow_unless: 1,
          allow_unless: 2,
          allow_only: 1,
          allow_only: 2,
          deny: 1,
          deny: 2,
          deny_unless: 1,
          deny_unless: 2,
          deny_only: 1,
          deny_only: 2
        ]

      unquote(block)
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Authorization.Rule, only: []
    end
  end

  defmacro defaults(:all) do
    quote do
      defaults([:create, :update, :destroy, :read])
    end
  end

  defmacro defaults(defaults, opts \\ []) do
    quote do
      opts = unquote(opts)

      for default <- unquote(defaults) do
        case default do
          :create ->
            create(:default, opts)

          :update ->
            update(:default, opts)

          :destroy ->
            destroy(:default, opts)

          :read ->
            read(:default, opts)

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
      action =
        Ash.Resource.Actions.Create.new(name,
          primary?: opts[:primary?] || false,
          rules: opts[:rules] || []
        )

      @actions action
    end
  end

  defmacro update(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action =
        Ash.Resource.Actions.Update.new(name,
          primary?: opts[:primary?] || false,
          rules: opts[:rules] || []
        )

      @actions action
    end
  end

  defmacro destroy(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action =
        Ash.Resource.Actions.Destroy.new(name,
          primary?: opts[:primary?] || false,
          rules: opts[:rules] || []
        )

      @actions action
    end
  end

  defmacro read(name, opts \\ []) do
    quote bind_quoted: [name: name, opts: opts] do
      action =
        Ash.Resource.Actions.Read.new(name,
          primary?: opts[:primary?] || false,
          rules: opts[:rules] || [],
          paginate?: Keyword.get(opts, :paginate?, true)
        )

      @actions action
    end
  end
end
