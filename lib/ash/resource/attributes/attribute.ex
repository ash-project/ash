defmodule Ash.Resource.Attributes.Attribute do
  @doc false

  defstruct [:name, :type, :primary_key?]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.type(),
          primary_key?: boolean()
        }

  @builtins Ash.Type.builtins()
  @schema Ashton.schema(opts: [primary_key?: :boolean], defaults: [primary_key?: false])

  @doc false
  def attribute_schema(), do: @schema

  def new(resource, name, type, opts \\ [])

  def new(resource, name, _, _) when not is_atom(name) do
    raise Ash.Error.ResourceDslError,
      resource: resource,
      message: "Attribute name must be an atom, got: #{inspect(name)}",
      path: [:attributes, :attribute]
  end

  def new(resource, _name, type, _opts) when not is_atom(type) do
    raise Ash.Error.ResourceDslError,
      resource: resource,
      message: "Attribute type must be a built in type or a type module, got: #{inspect(type)}",
      path: [:attributes, :attribute]
  end

  def new(resource, name, type, opts) when type in @builtins do
    case Ashton.validate(opts, @schema) do
      {:error, [{key, message} | _]} ->
        raise Ash.Error.ResourceDslError,
          resource: resource,
          message: message,
          path: [:attributes, :attribute],
          option: key

      {:ok, opts} ->
        %__MODULE__{
          name: name,
          type: type,
          primary_key?: opts[:primary_key?] || false
        }
    end
  end

  def new(resource, name, type, opts) do
    if Ash.Type.ash_type?(type) do
      %__MODULE__{
        name: name,
        type: type,
        primary_key?: opts[:primary_key?] || false
      }
    else
      raise Ash.Error.ResourceDslError,
        resource: resource,
        message: "Attribute type must be a built in type or a type module, got: #{inspect(type)}",
        path: [:attributes, :attribute]
    end
  end
end
