defmodule Ash.Resource do
  defmacro __using__(_opts) do
    quote do
      @actions [
        get: false
      ]

      @attributes [
        id: [ecto_type: :binary_id, type: :uuid]
      ]

      # Module.put_attribute(__MODULE__, :custom_threshold_for_lib, 10)
      import Ash.Resource
    end
  end

  defmacro actions(do: block) do
    quote do
      import Ash.Resource.Actions
      unquote(block)
      import Ash.Resource.Actions, only: []
    end
  end

  defmacro attributes(do: block) do
    quote do
      import Ash.Resource.Attributes
      unquote(block)
      import Ash.Resource.Attributes, only: []
    end
  end

  defmacro resource(name, do: block) do
    quote do
      unquote(block)
      require Ash.Resource.Schema

      Ash.Resource.Schema.define_schema(unquote(name))

      def actions() do
        @actions
      end

      def attributes() do
        @attributes
      end

      def name() do
        unquote(name)
      end
    end
  end
end
