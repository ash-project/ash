defmodule Ash.Resource.Attributes do
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
