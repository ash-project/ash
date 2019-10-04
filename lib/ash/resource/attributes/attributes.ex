defmodule Ash.Resource.Attributes do
  defmacro attributes(do: block) do
    quote do
      import Ash.Resource.Attributes
      unquote(block)
      import Ash.Resource.Attributes, only: [attributes: 1]
    end
  end

  defmacro attribute(name, type) do
    quote bind_quoted: [type: type, name: name] do
      @attributes Ash.Resource.Attributes.Attribute.new(name, type)
    end
  end
end
