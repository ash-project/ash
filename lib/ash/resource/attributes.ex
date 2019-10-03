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
      ecto_type =
        if type == :uuid do
          :binary_id
        else
          type
        end

      @attributes Keyword.put(@attributes, name, ecto_type: ecto_type, type: type)
    end
  end
end
