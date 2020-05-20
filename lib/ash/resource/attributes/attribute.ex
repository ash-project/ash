defmodule Ash.Resource.Attributes.Attribute do
  @doc false

  defstruct [
    :name,
    :type,
    :allow_nil?,
    :generated?,
    :primary_key?,
    :writable?,
    :default,
    :update_default
  ]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.type(),
          primary_key?: boolean(),
          default: (() -> term),
          update_default: (() -> term) | (Ash.record() -> term),
          writable?: boolean
        }

  @schema Ashton.schema(
            opts: [
              primary_key?: :boolean,
              allow_nil?: :boolean,
              generated?: :boolean,
              writable?: :boolean,
              update_default: [
                {:function, 0},
                {:function, 1},
                {:tuple, {:module, :atom}},
                {:tuple, {{:const, :constant}, :any}}
              ],
              default: [
                {:function, 0},
                {:tuple, {:module, :atom}},
                {:tuple, {{:const, :constant}, :any}}
              ]
            ],
            defaults: [
              primary_key?: false,
              generated?: false,
              allow_nil?: true,
              writable?: true
            ],
            describe: [
              allow_nil?: "#TODO: doc this",
              generated?: "#TODO: doc this",
              primary_key?: "#TODO: doc this",
              writable?: "#TODO: doc this",
              default: "#TODO: doc this"
            ]
          )

  @doc false
  def attribute_schema(), do: @schema

  @spec new(Ash.resource(), atom, Ash.Type.t(), Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(_resource, name, type, opts) do
    # Don't call functions on the resource! We don't want it to compile here
    with {:ok, opts} <- Ashton.validate(opts, @schema),
         {:default, {:ok, default}} <- {:default, cast_default(type, opts)} do
      {:ok,
       %__MODULE__{
         name: name,
         type: type,
         generated?: opts[:generated?],
         writable?: opts[:writable?],
         allow_nil?: opts[:allow_nil?],
         primary_key?: opts[:primary_key?],
         update_default: opts[:update_default],
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
