# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  @typedoc """
  A preparation implementation module or a `{module, opts}` tuple.

  The implementation receives the options after `c:init/1` normalizes them.
  """
  @type ref :: {module(), Keyword.t()} | module()

  @typedoc """
  A preparation declaration from the resource DSL, with its implementation,
  action types, and any conditions under which it runs.

  See [Preparations](/documentation/topics/resources/preparations.md).
  """
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

  @doc false
  @spec init(module(), Keyword.t()) :: {:ok, Keyword.t()} | {:error, term()}
  def init(module, opts) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :init,
      [opts],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "init/1"
    )
  end

  @doc false
  @spec prepare(
          module(),
          Ash.Query.t() | Ash.ActionInput.t(),
          Keyword.t(),
          Ash.Resource.Preparation.Context.t()
        ) :: Ash.Query.t() | Ash.ActionInput.t()
  def prepare(module, query_or_input, opts, context) do
    Ash.Temporal.assert_temporal_safe!(:preparation, module, opts, query_or_input)

    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :prepare,
      [query_or_input, opts, context],
      [%Ash.Query{}, %Ash.ActionInput{}],
      behaviour: __MODULE__,
      callback_name: "prepare/3"
    )
  end

  defmodule Context do
    @moduledoc """
    The context for a preparation.
    """
    defstruct [:actor, :tenant, :authorize?, :tracer, source_context: %{}]

    @typedoc """
    Action information passed to preparation callbacks, including the actor,
    tenant, and source context. Pass `scope: context` to a nested action to
    carry over the actor, tenant, and shared context.
    """
    @type t :: %__MODULE__{
            actor: Ash.Resource.Record.t() | nil,
            tenant: term(),
            authorize?: boolean() | nil,
            source_context: map(),
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil
          }
  end

  @doc """
  Validates and normalizes the configured preparation options.

  Return `{:ok, opts}` to pass the resulting options to `c:prepare/3`, or
  `{:error, error}` to reject them. The default accepts the supplied options.
  """
  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, term}
  @doc """
  Returns the query or action input after applying the preparation.

  For example, call `Ash.Query.sort/3` to add a default sort to a read query.
  Read action arguments with `Ash.Query.get_argument/2` or
  `Ash.ActionInput.get_argument/2`, depending on the input. `opts` contains
  the preparation's configured options.
  """
  @callback prepare(
              query_or_input :: Ash.Query.t() | Ash.ActionInput.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              Ash.Query.t() | Ash.ActionInput.t()
  @doc """
  Lists the input modules the preparation accepts for the supplied options.

  The default is `[Ash.Query]` for read actions. Include `Ash.ActionInput`
  to support generic actions and handle that struct in `c:prepare/3`.
  """
  @callback supports(opts :: Keyword.t()) :: [Ash.Query | Ash.ActionInput]

  @doc """
  Whether this preparation is safe to run as part of an action on a temporal resource.

  Every action on a [temporal resource](/documentation/topics/advanced/temporal-resources.md)
  runs "as of" a point in time, which may be in the past or the future. A preparation that
  runs there must not assume the action is happening now: it must not read the wall clock
  (use `now()` in expressions, or the subject's `as_of`), must not have side effects that
  assume the present, and must perform any reads through Ash so that `as_of` is threaded
  to them.

  Defaults to `false`. Running a preparation that is not temporal safe on a temporal
  resource raises `Ash.Error.Framework.NotTemporalSafe`. Return `true` to declare the
  preparation safe, inspecting `opts` if it is only safe for some configurations.
  """
  @callback temporal_safe?(opts :: Keyword.t()) :: boolean

  @optional_callbacks temporal_safe?: 1

  @doc """
  Sets up a module-based preparation with defaults for `c:init/1`,
  `c:supports/1`, and `c:temporal_safe?/1`.

  Implement `c:prepare/3` and register the module with `prepare` in the
  resource or action. Pass implementation options with `prepare {module, opts}`.
  The `use` macro takes no configuration options.

  See [Preparations](/documentation/topics/resources/preparations.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Preparation

      import Ash.Expr
      require Ash.Query

      def init(opts), do: {:ok, opts}
      def supports(_opts), do: [Ash.Query]
      def temporal_safe?(_opts), do: false

      defoverridable init: 1, supports: 1, temporal_safe?: 1
    end
  end
end
