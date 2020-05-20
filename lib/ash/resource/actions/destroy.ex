defmodule Ash.Resource.Actions.Destroy do
  @moduledoc "The representation of a `destroy` action"

  defstruct [:type, :name, :primary?]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
          primary?: boolean
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean
                ],
                defaults: [
                  primary?: false
                ],
                describe: [
                  primary?:
                    "Whether or not this action should be used when no action is specified by the caller."
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(Ash.resource(), atom, Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(_resource, name, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           type: :destroy,
           primary?: opts[:primary?]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
