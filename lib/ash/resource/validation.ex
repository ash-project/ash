# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation do
  @moduledoc """
  Represents a validation in Ash.

  See `Ash.Resource.Validation.Builtins` for a list of builtin validations.

  To write your own validation, define a module that implements the `c:init/1` callback
  to validate options at compile time, and `c:validate/3` callback to do the validation.

  Then, in a resource, you can say:

  ```
  validations do
    validate {MyValidation, [foo: :bar]}
  end
  ```
  """
  defstruct [
    :validation,
    :module,
    :opts,
    :only_when_valid?,
    :description,
    :message,
    :before_action?,
    :always_atomic?,
    where: [],
    on: [],
    __spark_metadata__: nil
  ]

  @typedoc """
  A validation declaration from the resource DSL. It records the validation
  implementation and configuration, including any conditions for running it.

  See [Validations](/documentation/topics/resources/validations.md).
  """
  @type t :: %__MODULE__{
          validation: {atom(), list(atom())},
          module: atom(),
          opts: list(atom()),
          only_when_valid?: boolean(),
          description: String.t() | nil,
          where: list({atom(), list(atom())}),
          on: list(atom()),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @type path :: [atom | integer]
  @typedoc """
  A validation implementation module or a `{module, opts}` tuple.

  The implementation receives the options after `c:init/1` normalizes them.
  """
  @type ref :: {module(), Keyword.t()} | module()

  defmodule Context do
    @moduledoc """
    Context for a validation.
    """
    defstruct [:actor, :tenant, :authorize?, :tracer, :message, bulk?: false, source_context: %{}]

    @typedoc """
    Action information passed to validation callbacks. `:message` holds a
    configured error message override, when present. `:bulk?` indicates
    that the validation is running in a bulk action.
    """
    @type t :: %__MODULE__{
            actor: Ash.Resource.Record.t() | nil,
            message: String.t() | nil,
            tenant: term(),
            source_context: map(),
            authorize?: boolean() | nil,
            tracer: Ash.Tracer.t() | [Ash.Tracer.t()] | nil,
            bulk?: boolean()
          }
  end

  @doc """
  Validates and normalizes options from a validation declaration.

  Return `{:ok, opts}` to pass the resulting options to the validation or
  `{:error, message}` to reject the configuration. The default accepts the
  supplied options unchanged.
  """
  @callback init(opts :: Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
  @doc """
  Lists the kinds of action input the validation accepts.

  The default is `[Ash.Changeset]`. Return `Ash.Query` or `Ash.ActionInput`
  in the list when the implementation also handles reads or generic actions.
  """
  @callback supports(opts :: Keyword.t()) :: [Ash.Changeset | Ash.Query | Ash.ActionInput]
  @doc """
  Checks an action input and returns `:ok` or `{:error, error}`.

  Use the subject's argument or attribute accessors to read values and
  return an Ash error that identifies the invalid field when applicable.
  `opts` contains the validation's configured options. See `c:atomic/3`
  for checks that need to run as part of an atomic update.
  """
  @callback validate(
              changeset_query_or_input :: Ash.Changeset.t() | Ash.ActionInput.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              :ok | {:error, term}
  @doc """
  Describes a validation failure for the supplied configuration.

  Return an error message string or a keyword list with `:message` and
  `:vars` for an interpolated error message.
  """
  @callback describe(opts :: Keyword.t()) ::
              String.t() | [{:message, String.t()} | {:vars, Keyword.t()}]
  @doc """
  Describes checks that can be evaluated as part of an atomic write.

  Return `:ok` when there is no check to apply, or return one or more
  `{:atomic, fields, condition, error}` tuples. When the condition is true,
  Ash evaluates the error expression. Use the field names involved in the
  check or `:*` for any field. Return `{:not_atomic, reason}` when the check
  cannot be expressed atomically, or `{:error, error}` when validation fails
  while building the atomic check.
  """
  @callback atomic(
              changeset_query_or_input :: Ash.Changeset.t() | Ash.ActionInput.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              :ok
              | {:atomic, involved_fields :: list(atom) | :*, condition_expr :: Ash.Expr.t(),
                 error_expr :: Ash.Expr.t()}
              | [
                  {:atomic, involved_fields :: list(atom) | :*, condition_expr :: Ash.Expr.t(),
                   error_expr :: Ash.Expr.t()}
                ]
              | {:not_atomic, String.t()}
              | {:error, term()}

  @doc """
  Replaces `validate/3` for batch actions, allowing to optimize validations for bulk actions.

  Receives all changesets in the batch and returns them with errors added to any that
  fail validation. Unlike `validate/3` which returns `:ok | {:error, term}`, this callback
  returns the changesets directly (with errors already added via `Ash.Changeset.add_error/2`).
  """
  @callback batch_validate(
              changesets :: [Ash.Changeset.t()],
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              Enumerable.t(Ash.Changeset.t())

  @doc """
  Whether or not batch callbacks should be run (if they are defined). Defaults to `true`.
  """
  @callback batch_callbacks?(
              changesets_or_query :: [Ash.Changeset.t()] | Ash.Query.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              boolean

  @doc """
  Reports whether the validation supports `c:atomic/3`.

  `use Ash.Resource.Validation` detects an implementation by default. A
  validation can override this callback to report its own capability.
  """
  @callback atomic?() :: boolean
  @doc """
  Reports whether a validation provides a single-input validation callback.

  `use Ash.Resource.Validation` defines this callback automatically.
  """
  @callback has_validate?() :: boolean
  @doc """
  Reports whether the module implements `c:batch_validate/3`.

  `use Ash.Resource.Validation` defines this callback automatically.
  """
  @callback has_batch_validate?() :: boolean

  @doc """
  Whether this validation is safe to run as part of an action on a temporal resource.

  Every action on a [temporal resource](/documentation/topics/advanced/temporal-resources.md)
  runs "as of" a point in time, which may be in the past or the future. A validation that
  runs there must not assume the action is happening now: it must not read the wall clock
  (use `now()` in expressions, or the subject's `as_of`), and must perform any reads
  through Ash so that `as_of` is threaded to them.

  Defaults to `false`. Running a validation that is not temporal safe on a temporal
  resource raises `Ash.Error.Framework.NotTemporalSafe`. Return `true` to declare the
  validation safe, inspecting `opts` if it is only safe for some configurations.
  """
  @callback temporal_safe?(opts :: Keyword.t()) :: boolean

  @optional_callbacks describe: 1,
                      validate: 3,
                      atomic: 3,
                      batch_validate: 3,
                      temporal_safe?: 1

  @validation_type {:spark_function_behaviour, Ash.Resource.Validation,
                    Ash.Resource.Validation.Builtins, {Ash.Resource.Validation.Function, 2}}

  @schema [
    validation: [
      type: @validation_type,
      required: true,
      doc:
        "The module (or module and opts) that implements the `Ash.Resource.Validation` behaviour. Also accepts a function that receives the changeset and its context."
    ],
    where: [
      type: {:wrap_list, @validation_type},
      required: false,
      default: [],
      doc: """
      Validations that should pass in order for this validation to apply. Any of these validations failing will result in this validation being ignored.
      """
    ],
    on: [
      type: {:wrap_list, {:in, [:create, :update, :destroy, :read, :action]}},
      default: [:create, :update],
      doc: """
      The action types the validation should run on. Many validations don't make sense in the context of destroy, read, or generic actions, so by default they are not included.
      """
    ],
    only_when_valid?: [
      type: :boolean,
      default: false,
      doc:
        "If the validation should only run on valid changesets. Useful for expensive validations or validations that depend on valid data."
    ],
    message: [
      type: :string,
      doc: "If provided, overrides any message set by the validation error"
    ],
    description: [
      type: :string,
      doc: "An optional description for the validation"
    ],
    before_action?: [
      type: :boolean,
      default: false,
      doc:
        "If set to `true`, the validation will be run in a before_action hook, i.e. when the action is executed (inside the transaction) rather than when the changeset is built. Useful for validations that run queries or are otherwise expensive. Cannot be used with atomic actions."
    ],
    always_atomic?: [
      type: :boolean,
      default: false,
      doc:
        "By default, validations are only run atomically if all changes will be run atomically or if there is no `validate/3` callback defined. Set this to `true` to run it atomically always."
    ]
  ]

  @action_schema Keyword.delete(@schema, :on)

  @doc """
  Sets up a module-based resource validation.

  Implement `c:validate/3`, `c:batch_validate/3`, or `c:atomic/3` for the
  execution paths you support. The default `c:supports/1` accepts changesets.
  Pass configuration in the resource with `validate {module, opts}`.
  The `use` macro takes no configuration options.

  See [Validations](/documentation/topics/resources/validations.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Validation
      @before_compile Ash.Resource.Validation

      import Ash.Expr
      require Ash.Query

      @impl true
      def init(opts), do: {:ok, opts}

      @impl true
      def supports(_opts), do: [Ash.Changeset]

      @impl true
      def batch_callbacks?(_, _, _), do: true

      @impl true
      def temporal_safe?(_opts), do: false

      defp with_description(keyword, opts) do
        if Kernel.function_exported?(__MODULE__, :describe, 1) do
          keyword ++ Ash.Resource.Validation.describe(__MODULE__, opts)
        else
          keyword
        end
      end

      defoverridable init: 1, supports: 1, batch_callbacks?: 3, temporal_safe?: 1
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:validate, 2}, :def) or
           Module.defines?(__MODULE__, {:validate, 3}, :def) do
        @impl true
        def has_validate?, do: true
      else
        @impl true
        def has_validate?, do: false
      end

      if Module.defines?(__MODULE__, {:batch_validate, 3}, :def) do
        @impl true
        def has_batch_validate?, do: true
      else
        @impl true
        def has_batch_validate?, do: false
      end

      if Module.defines?(__MODULE__, {:atomic, 3}, :def) do
        if !Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
          @impl true
          def atomic?, do: true
        end
      else
        if !Module.defines?(__MODULE__, {:atomic?, 0}, :def) do
          @impl true
          def atomic?, do: false
        end

        @impl true
        def atomic(_changeset, _opts, _context),
          do: {:not_atomic, "#{inspect(__MODULE__)} does not implement `atomic/3`"}
      end
    end
  end

  @doc false
  def transform(%{validation: {module, opts}} = validation) do
    opts =
      Enum.map(opts, fn
        {key, %Regex{} = value} when module == Ash.Resource.Validation.Match ->
          source = Regex.source(value)
          opts = Regex.opts(value)
          {key, {Spark.Regex, :cache, [source, opts]}}

        {key, value} ->
          {key, value}
      end)

    {:ok,
     %{
       validation
       | validation: {module, opts},
         module: module,
         opts: opts
     }}
  end

  @doc false
  @spec validate(
          module(),
          Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t(),
          Keyword.t(),
          Context.t()
        ) :: :ok | {:error, term()}
  def validate(module, changeset_query_or_input, opts, context) do
    Ash.Temporal.assert_temporal_safe!(:validation, module, opts, changeset_query_or_input)

    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :validate,
      [changeset_query_or_input, opts, context],
      [:ok, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "validate/3"
    )
  end

  @doc false
  @spec describe(module(), Keyword.t() | map()) ::
          String.t() | [{:message, String.t()} | {:vars, Keyword.t()}]
  def describe(module, opts) do
    result = apply(module, :describe, [opts])

    if is_binary(result) or (is_list(result) and Keyword.keyword?(result)) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.describe/1.

        The callback #{inspect(__MODULE__)}.describe/1 expects a String.t() or a keyword list of :message/:vars.
        """
    end
  end

  @doc false
  @spec init(module(), Keyword.t()) :: {:ok, Keyword.t()} | {:error, String.t()}
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
  @spec atomic(
          module(),
          Ash.Changeset.t() | Ash.ActionInput.t(),
          Keyword.t(),
          Context.t()
        ) ::
          :ok
          | {:atomic, list(atom()) | :*, Ash.Expr.t(), Ash.Expr.t()}
          | [{:atomic, list(atom()) | :*, Ash.Expr.t(), Ash.Expr.t()}]
          | {:not_atomic, String.t()}
          | {:error, term()}
  def atomic(module, changeset_query_or_input, opts, context) do
    Ash.Temporal.assert_temporal_safe!(:validation, module, opts, changeset_query_or_input)

    result = apply(module, :atomic, [changeset_query_or_input, opts, context])

    if valid_atomic_result?(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.atomic/3.

        The callback #{inspect(__MODULE__)}.atomic/3 expects one of the following return types:

          :ok
          {:atomic, involved_fields, condition_expr, error_expr}
          [list of {:atomic, involved_fields, condition_expr, error_expr}]
          {:not_atomic, String.t()}
          {:error, term()}
        """
    end
  end

  defp valid_atomic_result?(:ok), do: true
  defp valid_atomic_result?({:atomic, _, _, _}), do: true
  defp valid_atomic_result?({:not_atomic, _}), do: true
  defp valid_atomic_result?({:error, _}), do: true

  defp valid_atomic_result?(list) when is_list(list) do
    Enum.all?(list, &match?({:atomic, _, _, _}, &1))
  end

  defp valid_atomic_result?(_), do: false

  def opt_schema, do: @schema
  def action_schema, do: @action_schema
  def validation_type, do: @validation_type

  @doc false
  def maybe_redact(subject, field, value) do
    if should_redact?(subject, field) do
      Ash.Helpers.redact(value)
    else
      value
    end
  end

  @doc false
  def should_redact?(subject, field) do
    Application.get_env(:ash, :redact_sensitive_values_in_errors?, false) &&
      sensitive?(subject, field)
  end

  @doc false
  def sensitive?(subject, field) do
    argument_sensitive? =
      (subject.action.arguments || [])
      |> Enum.find(&(&1.name == field))
      |> case do
        %{sensitive?: true} -> true
        _ -> false
      end

    attribute_sensitive? =
      case Ash.Resource.Info.attribute(subject.resource, field) do
        %{sensitive?: true} -> true
        _ -> false
      end

    argument_sensitive? or attribute_sensitive?
  end
end
