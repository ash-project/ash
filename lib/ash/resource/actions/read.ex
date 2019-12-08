defmodule Ash.Resource.Actions.Read do
  @moduledoc "The representation of a `read` action"

  defstruct [:type, :name, :primary?, :authorization_steps, :paginate?]

  alias Ash.Authorization.Rule

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          paginate?: boolean,
          authorization_steps: list(Rule.t())
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean,
                  paginate?: :boolean,
                  authorization_steps: {:list, %Rule{}}
                ],
                defaults: [
                  primary?: false,
                  paginate?: true,
                  authorization_steps: []
                ],
                describe: [
                  primary?:
                    "Whether or not this action should be used when no action is specified by the caller.",
                  paginate?:
                    "If false, a page is still returned from a read action, but no limit or offset is performed.",
                  authorization_steps:
                    "A list of `Ash.Authorization.Rule`s that will be stepped through and applied in order."
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
           authorization_steps: opts[:authorization_steps],
           paginate?: opts[:paginate?]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
