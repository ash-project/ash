# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Calculation do
  @moduledoc """
  The behaviour for defining a module calculation, and the struct for storing a defined calculation.
  """

  defstruct allow_nil?: true,
            arguments: [],
            calculation: nil,
            constraints: [],
            description: nil,
            field?: true,
            filterable?: true,
            sortable?: true,
            sensitive?: false,
            authorize?: false,
            multitenancy: nil,
            load: [],
            name: nil,
            public?: false,
            async?: false,
            type: nil,
            __spark_metadata__: nil

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The field name to use for the calculation value"
    ],
    type: [
      type: :any,
      doc: "The type of the calculation. See `Ash.Type` for more.",
      required: true
    ],
    async?: [
      type: :boolean,
      default: false
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints to provide to the type. See `Ash.Type` for more."
    ],
    calculation: [
      type: Ash.OptionsHelpers.calculation_type(),
      required: true,
      doc: """
      The `module`, `{module, opts}` or `expr(...)` to use for the calculation. Also accepts a function that takes *a list of records* and the context, and produces a result for each record.
      """
    ],
    description: [
      type: :string,
      doc: "An optional description for the calculation"
    ],
    public?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the calculation will appear in public interfaces.
      """
    ],
    sensitive?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not references to the calculation will be considered sensitive.
      """
    ],
    authorize?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the related data that the calculation uses should be authorized, when the parent query is authorized.

      By default, calculations are trusted, just like `Ash.Query.filter/2`: relationship references in their expressions, and relationships they load, are not subject to the related resource's read policies. When `true`, the calculation only sees related records the actor can read:

      - In an expression, a reference through a relationship resolves to `nil` when the related record is not readable, and `exists` only considers readable related records.
      - Relationships that the calculation loads are loaded with authorization.

      Aggregates are authorized regardless of this option, according to their own `authorize?` option.
      """
    ],
    load: [
      type: :any,
      default: [],
      doc:
        "A load statement to be applied if the calculation is used. Only works with module-based or function-based calculations, not expression calculations."
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the calculation can return nil.

      When `false`, the calculation must never return `nil`. Ash does not validate the result, so a `nil` will not raise an error.
      """
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      doc: "Whether or not the calculation should be usable in filters."
    ],
    sortable?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the calculation can be referenced in sorts.
      """
    ],
    field?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the calculation should create a field on the resource struct.

      When `false`, the calculation's value will always be stored in the `calculations` map on the record,
      and will not add a key to the resource struct. The calculation can still be loaded normally.
      """
    ],
    multitenancy: [
      type: {:in, [:enforce, :allow_global, :bypass, :bypass_all]},
      doc: """
      Configures multitenancy behavior for the calculation.
      `:enforce` requires a tenant to be set (the default behavior),
      `:allow_global` allows using this calculation both with and without a tenant,
      `:bypass` completely ignores the tenant even if it's set,
      `:bypass_all` like `:bypass` but also bypasses the tenancy requirement for nested resources.
      """
    ]
  ]

  defmodule Context do
    @moduledoc """
    The context and arguments of a calculation
    """

    defstruct [
      :actor,
      :tenant,
      :authorize?,
      :tracer,
      :domain,
      :resource,
      :type,
      :constraints,
      :arguments,
      source_context: %{}
    ]

    @type t :: %__MODULE__{
            actor: term | nil,
            tenant: term(),
            authorize?: boolean,
            tracer: module | nil,
            source_context: map(),
            resource: module(),
            type: Ash.Type.t(),
            constraints: Keyword.t(),
            domain: module(),
            arguments: map()
          }
  end

  @type t :: %__MODULE__{
          allow_nil?: boolean,
          arguments: [__MODULE__.Argument.t()],
          calculation: module | {module, keyword},
          constraints: keyword,
          async?: boolean,
          description: nil | String.t(),
          field?: boolean,
          filterable?: boolean,
          load: keyword,
          sortable?: boolean,
          sensitive?: boolean,
          authorize?: boolean,
          multitenancy: nil | :enforce | :allow_global | :bypass | :bypass_all,
          name: atom(),
          public?: boolean,
          type: nil | Ash.Type.t(),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @type ref :: {module(), Keyword.t()} | module()

  @doc """
  Sets up a module-based calculation.

  Defines defaults for `c:init/1`, `c:describe/1`, `c:load/3` and
  `c:strict_loads?/0`, and imports `Ash.Expr`. Implement `c:calculate/3`,
  `c:expression/2`, or both. The macro takes no configuration options. Pass
  calculation-specific options in the resource as `{module, opts}`.

  See [Calculations](/documentation/topics/resources/calculations.md) for how
  to register and load a module-based calculation.
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Calculation

      @before_compile Ash.Resource.Calculation

      import Ash.Expr

      def init(opts), do: {:ok, opts}

      def describe(opts), do: "##{inspect(__MODULE__)}<#{inspect(opts)}>"

      def load(_query, _opts, _context), do: []

      defoverridable init: 1, describe: 1, load: 3
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:expression, 2}) do
        def has_expression?, do: true
      else
        def has_expression?, do: false
      end

      if Module.defines?(__MODULE__, {:calculate, 3}) do
        def has_calculate?, do: true
      else
        def has_calculate?, do: false
      end

      def strict_loads?, do: true

      defoverridable strict_loads?: 0
    end
  end

  @type opts :: Keyword.t()

  @doc """
  Validates or normalizes the options provided when declaring the calculation.

  Return `{:ok, opts}` with the options the calculation should use, or
  `{:error, error}` to reject the configuration. The default returns the
  supplied options unchanged. These options configure the implementation.
  Arguments supplied when loading a calculation are in `context.arguments`.
  """
  @callback init(opts :: opts) :: {:ok, opts} | {:error, term}

  @doc """
  Returns a string describing the calculation with its configured options.

  The default includes the module name and inspected options.
  """
  @callback describe(opts :: opts) :: String.t()

  @doc """
  Computes a value for each input record, in the same order as the records.

  Return the list of values directly or as `{:ok, values}`. Return
  `{:error, error}` if the calculation fails or `:unknown` if the values
  cannot be determined. Declare the fields the calculation reads in
  `c:load/3` and access calculation arguments through `context.arguments`.

  ## Examples

      @impl true
      def calculate(records, _opts, _context) do
        Enum.map(records, &String.upcase(&1.title))
      end

  This implementation expects `:title` to be loaded and to contain a string.
  """
  @callback calculate(records :: [Ash.Resource.Record.t()], opts :: opts, context :: Context.t()) ::
              {:ok, [term]} | [term] | {:error, term} | :unknown

  @doc """
  Returns an Ash expression defining the calculation.

  Use `Ash.Expr.expr/1` to build the expression. Ash can evaluate the
  expression in a supporting data layer, including when the calculation is
  used in a filter or sort.

  ## Examples

      @impl true
      def expression(_opts, _context), do: expr(first_name <> " " <> last_name)

  See [Expressions](/documentation/topics/reference/expressions.md) for
  supported expression syntax.
  """
  @callback expression(opts :: opts, context :: Context.t()) :: any
  @doc """
  A load statement to be applied when the calculation is used.
  Only works with module-based or function-based calculations, not expression calculations.
  """
  @callback load(query :: Ash.Query.t(), opts :: opts, context :: Context.t()) ::
              atom | [atom] | Keyword.t()

  @doc """
  Controls field selection when Ash loads the calculation's dependencies.

  The default is `true`. Ash uses strict loads for the calculation, including
  nested relationships. Declare the attributes the calculation reads in
  `c:load/3` or in the calculation's resource DSL `:load` option. For example,
  `[author: [:name]]` requests the author's name as a dependency.

  Return `false` to use normal field selection on dependency loads. For
  example, returning `[:author]` from `c:load/3` then loads the author's
  default selection of attributes.

  ## Examples

      @impl true
      def load(_query, _opts, _context), do: [:author]

      @impl true
      def strict_loads?, do: false

  ## See also

  - `c:load/3` for declaring calculation dependencies
  - `Ash.Query.load/3` for how the `:strict?` load option works
  - [Calculations](/documentation/topics/resources/calculations.md)
  """
  @callback strict_loads?() :: boolean()

  @doc """
  Reports whether the calculation module implements `c:expression/2`.

  `use Ash.Resource.Calculation` defines this callback automatically.
  """
  @callback has_expression?() :: boolean()

  @optional_callbacks expression: 2, calculate: 3

  @doc """
  Returns the option schema for calculation definitions in the resource DSL.

  See `d:Ash.Resource.Dsl.calculations.calculate` for the supported options
  when declaring a calculation.
  """
  def schema, do: @schema

  @doc false
  @spec init(module(), opts) :: {:ok, opts} | {:error, term}
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
  @spec describe(module(), opts) :: String.t()
  def describe(module, opts) do
    result = apply(module, :describe, [opts])

    if is_binary(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.describe/1.

        The callback #{inspect(__MODULE__)}.describe/1 expects a String.t().
        """
    end
  end

  @doc false
  @spec calculate(module(), [Ash.Resource.Record.t()], opts, Context.t()) ::
          {:ok, [term()]} | [term()] | {:error, term()} | :unknown
  def calculate(module, records, opts, context) do
    result = apply(module, :calculate, [records, opts, context])

    if match?({:ok, _}, result) or is_list(result) or match?({:error, _}, result) or
         result == :unknown do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.calculate/3.

        The callback #{inspect(__MODULE__)}.calculate/3 expects {:ok, [term()]}, [term()], {:error, term()}, or :unknown.
        """
    end
  end

  @doc false
  @spec expression(module(), opts, Context.t()) :: any()
  def expression(module, opts, context) do
    module
    |> apply(:expression, [opts, context])
    |> fill_arguments(context)
  end

  # A calculation's arguments belong to the calculation itself, so they are always
  # safe (and necessary) to resolve here. Callers fill in the ambient `actor`,
  # `tenant` and `context` templates themselves, because those are not always known
  # at the time the expression is built.
  defp fill_arguments(expression, context) do
    case context do
      %{arguments: arguments} when is_map(arguments) and arguments != %{} ->
        Ash.Expr.walk_template(expression, fn
          {:_arg, field} ->
            case Map.fetch(arguments, field) do
              {:ok, value} -> value
              :error -> Map.get(arguments, to_string(field))
            end

          other ->
            other
        end)

      _ ->
        expression
    end
  end

  @doc false
  @spec load(module(), Ash.Query.t(), opts, Context.t() | map()) ::
          atom() | [atom()] | Keyword.t()
  def load(module, query, opts, context) do
    result = apply(module, :load, [query, opts, context])

    # Accept atom or list (implementations may return list of atoms, keyword list, or list of refs)
    if is_atom(result) or is_list(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.load/3.

        The callback #{inspect(__MODULE__)}.load/3 expects an atom or a list.
        """
    end
  end

  @doc false
  @spec has_expression?(module()) :: boolean()
  def has_expression?(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :has_expression?,
      [],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "has_expression?/0"
    )
  end

  @doc false
  @spec has_calculate?(module()) :: boolean()
  def has_calculate?(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :has_calculate?,
      [],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "has_calculate?/0"
    )
  end

  @doc false
  @spec strict_loads?(module()) :: boolean()
  def strict_loads?(module) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :strict_loads?,
      [],
      [true, false],
      behaviour: __MODULE__,
      callback_name: "strict_loads?/0"
    )
  end

  @doc false
  def expr_calc(expr) when is_function(expr) do
    {:error,
     "Inline function calculations expect a function with arity 2. Got #{Function.info(expr)[:arity]}"}
  end

  def expr_calc(expr) do
    {:ok, {Ash.Resource.Calculation.Expression, expr: expr}}
  end
end
