defmodule Ash.Resource.Actions.Read do
  @moduledoc "The representation of a `read` action"

  defstruct [:type, :name, :primary?, :rules, :paginate?]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          paginate?: boolean,
          rules: Authorization.steps()
        }

  @opt_schema Ashton.schema(
                opts: [
                  primary?: :boolean,
                  paginate?: :boolean,
                  rules: :keyword
                ],
                defaults: [
                  primary?: false,
                  paginate?: true,
                  rules: []
                ],
                describe: [
                  primary?:
                    "Whether or not this action should be used when no action is specified by the caller.",
                  paginate?:
                    "If false, a page is still returned from a read action, but no limit or offset is performed.",
                  # TODO: doc better
                  rules: "A list of authorization steps"
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(Ash.resource(), atom, Keyword.t()) :: {:ok, t()} | {:error, term}
  def new(resource, name, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        rules =
          case opts[:rules] do
            false ->
              false

            steps ->
              base_attribute_opts = [
                resource: resource
              ]

              Enum.map(steps, fn {step, {mod, opts}} ->
                {step, {mod, Keyword.merge(base_attribute_opts, opts)}}
              end)
          end

        {:ok,
         %__MODULE__{
           name: name,
           type: :read,
           primary?: opts[:primary?],
           rules: rules,
           paginate?: opts[:paginate?]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
