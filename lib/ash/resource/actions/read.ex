defmodule Ash.Resource.Actions.Read do
  @moduledoc "The representation of a `read` action"

  defstruct [:type, :name, :primary?, :rules, :paginate?]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          rules: list(Ash.Authorization.Rule.t()),
          paginate?: boolean
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean,
                  rules: {:list, {:struct, Ash.Authorization.Rule}},
                  paginate?: :boolean
                ],
                defaults: [
                  primary?: false,
                  rules: [],
                  paginate?: true
                ],
                describe: [
                  primary?:
                    "Whether or not this action should be used when no action is specified by the caller.",
                  rules:
                    "A list of `Ash.Authorization.Rule`s declaring the authorization of the action.",
                  paginate?:
                    "If false, a page is still returned from a read action, but no limit or offset is performed."
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
           type: :read,
           primary?: opts[:primary?],
           rules: opts[:rules],
           paginate?: opts[:paginate?]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
