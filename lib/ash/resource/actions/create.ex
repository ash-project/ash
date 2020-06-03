defmodule Ash.Resource.Actions.Create do
  @moduledoc false
  defstruct [:type, :name, :primary?]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          primary?: boolean
        }

  @opt_schema [
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller."
    ]
  ]

  @doc false
  def opt_schema, do: @opt_schema

  @spec new(Ash.resource(), atom, Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(_resource, name, opts \\ []) do
    case NimbleOptions.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           type: :create,
           primary?: opts[:primary?]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
