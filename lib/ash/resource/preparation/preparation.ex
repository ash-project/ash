defmodule Ash.Resource.Preparation do
  @moduledoc """
  The behaviour for an action-specific query preparation.

  `c:init/1` is defined automatically by `use Ash.Resource.Preparation`, but can be implemented if you want to validate/transform any
  options passed to the module.

  The main function is `c:prepare/3`. It takes the query, any options that were provided
  when this preparation was configured on a resource, and the context, which currently only has
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
        The module and options for a preparation. Also accepts functions take the query and the context.
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

  defmodule Context do
    @moduledoc """
    The context for a preparation.
    """
    defstruct [:actor, :tenant, :authorize?, :tracer]

    @type t :: %__MODULE__{
            actor: Ash.Resource.record() | nil,
            tenant: term(),
            authorize?: boolean() | nil,
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil
          }
  end

  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback prepare(query :: Ash.Query.t(), opts :: Keyword.t(), context :: Context.t()) ::
              Ash.Query.t()

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Preparation

      import Ash.Expr
      require Ash.Query

      def init(opts), do: {:ok, opts}

      defoverridable init: 1
    end
  end
end
