defmodule Ash.Resource.Attributes.Attribute do
  @doc false

  defstruct [:name, :type, :allow_nil?, :primary_key?, :default, :authorization_steps]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.type(),
          primary_key?: boolean(),
          default: (() -> term),
          authorization_steps: Keyword.t()
        }

  @schema Ashton.schema(
            opts: [
              primary_key?: :boolean,
              allow_nil?: :boolean,
              authorization_steps: [{:const, false}, :keyword],
              default: [
                {:function, 0},
                {:tuple, {:module, :atom}},
                {:tuple, {{:const, :constant}, :any}}
              ]
            ],
            defaults: [
              primary_key?: false,
              allow_nil?: true,
              authorization_steps: []
            ],
            describe: [
              authorization_steps: """
              Rules applied on an attribute during create or update. If no rules are defined, authorization to change will fail.
              If set to false, no rules are applied and any changes are allowed (assuming the action was authorized as a whole)
              """,
              allow_nil?: """
              Whether or not to allow `null` values. Ash can perform optimizations with this information, so if you do not
              expect any null values, make sure to set this switch.
              """,
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
      authorization_steps =
        case opts[:authorization_steps] do
          false ->
            false

          steps ->
            base_attribute_opts = [
              attribute_name: name,
              attribute_type: type
            ]

            Enum.map(steps, fn {step, {mod, opts}} ->
              {step, {mod, Keyword.merge(base_attribute_opts, opts)}}
            end)
        end

      {:ok,
       %__MODULE__{
         name: name,
         type: type,
         authorization_steps: authorization_steps,
         allow_nil?: opts[:allow_nil?],
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
