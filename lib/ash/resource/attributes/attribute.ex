defmodule Ash.Resource.Attributes.Attribute do
  @doc false

  defstruct [:name, :type, :primary_key?, :default]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.type(),
          primary_key?: boolean(),
          default: (() -> term)
        }

  @schema Ashton.schema(
            opts: [
              primary_key?: :boolean,
              default: [
                {:function, 0},
                {:tuple, {:module, :atom}},
                {:tuple, {{:const, :constant}, :any}}
              ]
            ],
            defaults: [
              primary_key?: false
            ],
            describe: [
              primary_key?:
                "Whether this field is, or is part of, the primary key of a resource.",
              default:
                "A one argument function that returns a default value, an mfa that does the same, or a raw value via specifying `{:constant, value}`."
            ]
          )

  @doc false
  def attribute_schema(), do: @schema

  @spec new(atom, Ash.Type.t(), Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(name, type, opts) do
    with {:ok, opts} <- Ashton.validate(opts, @schema),
         {:default, {:ok, default}} <- {:default, cast_default(type, opts)} do
      {:ok,
       %__MODULE__{
         name: name,
         type: type,
         primary_key?: opts[:primary_key?],
         default: default
       }}
    else
      {:error, error} -> {:error, error}
      {:default, _} -> {:error, [{:default, "is not a valid default for type #{inspect(type)}"}]}
    end
  end

  defp cast_default(type, opts) do
    case Keyword.fetch(opts, :default) do
      {:ok, default} when is_function(default, 0) ->
        {:ok, default}

      {:ok, {mod, func}} when is_atom(mod) and is_atom(func) ->
        {:ok, {mod, func}}

      {:ok, {:constant, default}} ->
        case Ash.Type.cast_input(type, default) do
          {:ok, value} -> {:ok, {:constant, value}}
          :error -> :error
        end

      :error ->
        {:ok, nil}
    end
  end
end
