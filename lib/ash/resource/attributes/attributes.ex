defmodule Ash.Resource.Attributes do
  defmacro attributes(do: block) do
    quote do
      import Ash.Resource.Attributes
      unquote(block)
      import Ash.Resource.Attributes, only: [attributes: 1]
    end
  end

  defmacro attribute(name, type, opts \\ []) do
    quote bind_quoted: [type: type, name: name, opts: opts] do
      @attributes Ash.Resource.Attributes.Attribute.new(__MODULE__, name, type, opts)
    end
  end
end
