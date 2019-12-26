defmodule Ash.Resource.Attributes do
  @moduledoc """
  A DSL component for declaring attributes

  Attributes are fields on an instance of a resource. The two required
  pieces of knowledge are the field name, and the type.
  """

  @doc false
  defmacro attributes(do: block) do
    quote do
      import Ash.Resource.Attributes
      unquote(block)
      import Ash.Resource.Attributes, only: [attributes: 1]
    end
  end

  @doc """
  Declares an attribute on the resource

  Type can be either a built in type (see `Ash.Type`) for more, or a module
  implementing the `Ash.Type` behaviour.

  #{Ashton.document(Ash.Resource.Attributes.Attribute.attribute_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  attribute :first_name, :string, primary_key?: true
  ```
  """
  defmacro attribute(name, type, opts \\ []) do
    quote bind_quoted: [type: type, name: name, opts: opts] do
      unless is_atom(name) do
        raise Ash.Error.ResourceDslError,
          message: "Attribute name must be an atom, got: #{inspect(name)}",
          path: [:attributes, :attribute]
      end

      unless is_atom(type) do
        raise Ash.Error.ResourceDslError,
          message:
            "Attribute type must be a built in type or a type module, got: #{inspect(type)}",
          path: [:attributes, :attribute, name]
      end

      type = Ash.Type.get_type(type)

      unless type in Ash.Type.builtins() or Ash.Type.ash_type?(type) do
        raise Ash.Error.ResourceDslError,
          message:
            "Attribute type must be a built in type or a type module, got: #{inspect(type)}",
          path: [:attributes, :attribute, name]
      end

      case Ash.Resource.Attributes.Attribute.new(name, type, opts) do
        {:ok, attribute} ->
          @attributes attribute

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            path: [:attributes, :attribute],
            option: key
      end
    end
  end
end
