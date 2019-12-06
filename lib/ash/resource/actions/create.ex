defmodule Ash.Resource.Actions.Create do
  @moduledoc "The representation of a `create` action."
  defstruct [:type, :name, :primary?, :rules]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          primary?: boolean,
          rules: list(Ash.Authorization.Rule.t())
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean,
                  rules: {:list, {:struct, Ash.Authorization.Rule}}
                ],
                defaults: [
                  primary?: false,
                  rules: []
                ],
                describe: [
                  primary?:
                    "Whether or not this action should be used when no action is specified by the caller.",
                  rules:
                    "A list of `Ash.Authorization.Rule`s declaring the authorization of the action."
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(atom, Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(name, opts \\ []) do
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           type: :create,
           primary?: opts[:primary?],
           rules: opts[:rules]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
