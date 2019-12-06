defmodule Ash.Resource.Attributes.Attribute do
  @doc false

  defstruct [:name, :type, :primary_key?]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.type(),
          primary_key?: boolean()
        }

  @schema Ashton.schema(
            opts: [primary_key?: :boolean],
            defaults: [primary_key?: false],
            describe: [
              primary_key?: "Whether this field is, or is part of, the primary key of a resource."
            ]
          )

  @doc false
  def attribute_schema(), do: @schema

  @spec new(atom, Ash.Type.t(), Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(name, type, opts) do
    case Ashton.validate(opts, @schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           type: type,
           primary_key?: opts[:primary_key?] || false
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
