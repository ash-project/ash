defmodule Ash.Resource.Preparation do
  @moduledoc """
  The behaviour for an action-specific query preparation.

  To implement one, simply implement the behaviour. `c:init/1` is defined automatically
  by `use Ash.Resource.Preparation`, but can be implemented if you want to validate/transform any
  options passed to the module.

  The main function is `c:prepare/3`. It takes the changeset, any options that were provided
  when this change was configured on a resource, and the context, which currently only has
  the actor.

  To access any query arguments from within a preparation, make sure you are using `Ash.Query.get_argument/2`
  as the argument keys may be strings or atoms.
  """
  defstruct [:preparation]

  @type t :: %__MODULE__{}

  @doc false
  def schema do
    [
      preparation: [
        type: {:ash_behaviour, Ash.Resource.Preparation, Ash.Resource.Preparation.Builtins},
        doc: """
        The module and options for a preparation.
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

  @doc false
  def transform(%{preparation: {module, opts}} = preparation) do
    case module.init(opts) do
      {:ok, opts} -> {:ok, %{preparation | preparation: {module, opts}}}
      {:error, error} -> {:error, error}
    end
  end

  @type context :: %{actor: Ash.Resource.record()} | %{}
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
