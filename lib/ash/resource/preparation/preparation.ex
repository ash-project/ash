defmodule Ash.Resource.Preparation do
  @moduledoc """
  The behaviour for an action-specific query preparation.

  `c:init/1` is defined automatically by `use Ash.Resource.Preparation`, but can be implemented if you want to validate/transform any
  options passed to the module.

  The main function is `c:prepare/3`. It takes the changeset, any options that were provided
  when this change was configured on a resource, and the context, which currently only has
  the actor.

  To access any query arguments from within a preparation, make sure you are using `Ash.Query.get_argument/2`
  as the argument keys may be strings or atoms.
  """
  defstruct [:preparation]

  @type t :: %__MODULE__{}
  @type ref :: {module(), Keyword.t()} | module()

  @doc false
  def schema do
    [
      preparation: [
        type:
          {:spark_function_behaviour, Ash.Resource.Preparation, Ash.Resource.Preparation.Builtins,
           {Ash.Resource.Preparation.Function, 2}},
        doc: """
        The module and options for a preparation.
        Also accepts functions take the query and the context.
        """,
        required: true
      ]
    ]
  end

  @doc false
  def preparation({module, opts}) when is_atom(module) do
    if Keyword.keyword?(opts) do
      {:ok, {module, opts}}
    else
      {:error, "Expected opts to be a keyword, got: #{inspect(opts)}"}
    end
  end

  def preparation(module) when is_atom(module), do: {:ok, {module, []}}

  def preparation(other) do
    {:error, "Expected a module and opts, got: #{inspect(other)}"}
  end

  @type context :: %{
          optional(:actor) => Ash.Resource.record(),
          optional(any) => any
        }
  @callback init(Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback prepare(query, Keyword.t(), context) :: query when query: struct

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Preparation

      def init(opts), do: {:ok, opts}

      defoverridable init: 1
    end
  end
end
