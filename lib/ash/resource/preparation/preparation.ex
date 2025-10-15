# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
  defstruct [
    :preparation,
    only_when_valid?: false,
    where: [],
    on: [:read],
    __spark_metadata__: nil
  ]

  require Ash.BehaviourHelpers

  @type ref :: {module(), Keyword.t()} | module()

  @type t :: %__MODULE__{
          preparation: __MODULE__.ref(),
          only_when_valid?: boolean(),
          where: [Ash.Resource.Validation.ref()],
          on: [atom()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

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
      ],
      on: [
        type: {:wrap_list, {:in, [:read, :action]}},
        default: [:read],
        doc: """
        The action types the preparation should run on. By default, preparations only run on read actions. Use `:action` to run on generic actions.
        """
      ],
      where: [
        type:
          {:wrap_list,
           {:spark_function_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins,
            {Ash.Resource.Validation.Function, 2}}},
        required: false,
        default: [],
        doc: """
        Validations that should pass in order for this preparation to apply. Any of these validations failing will result in this preparation being ignored.
        """
      ],
      only_when_valid?: [
        type: :boolean,
        default: false,
        doc: "If the preparation should only run on valid queries."
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

  def prepare(module, query_or_input, opts, context) do
    Ash.BehaviourHelpers.check_type!(module, module.prepare(query_or_input, opts, context), [
      %Ash.Query{},
      %Ash.ActionInput{}
    ])
  end

  defmodule Context do
    @moduledoc """
    The context for a preparation.
    """
    defstruct [:actor, :tenant, :authorize?, :tracer, source_context: %{}]

    @type t :: %__MODULE__{
            actor: Ash.Resource.record() | nil,
            tenant: term(),
            authorize?: boolean() | nil,
            source_context: map(),
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil
          }
  end

  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @callback prepare(
              query_or_input :: Ash.Query.t() | Ash.ActionInput.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              Ash.Query.t() | Ash.ActionInput.t()
  @callback supports(opts :: Keyword.t()) :: [module()]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Preparation

      import Ash.Expr
      require Ash.Query

      def init(opts), do: {:ok, opts}
      def supports(_opts), do: [Ash.Query]

      defoverridable init: 1, supports: 1
    end
  end
end
