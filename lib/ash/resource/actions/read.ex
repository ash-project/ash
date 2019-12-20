defmodule Ash.Resource.Actions.Read do
  @moduledoc "The representation of a `read` action"

  defstruct [:type, :name, :primary?, :authorization_steps, :paginate?]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          paginate?: boolean,
          authorization_steps: Authorization.steps()
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean,
                  paginate?: :boolean,
                  authorization_steps: {:list, :any}
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
                  # TODO: doc better
                  authorization_steps: "A list of authorization steps"
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
