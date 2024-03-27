defmodule Ash.Resource.Calculation do
  @moduledoc """
  The behaviour for defining a module calculation, and the struct for storing a defined calculation.
  """

  defstruct allow_nil?: true,
            arguments: [],
            calculation: nil,
            constraints: [],
            description: nil,
            filterable?: true,
            sortable?: true,
            load: [],
            name: nil,
            public?: false,
            sensitive?: false,
            type: nil

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
    constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints to provide to the type. See `Ash.Type` for more."
    ],
    calculation: [
      type:
        {:or,
         [
           {:spark_function_behaviour, Ash.Resource.Calculation,
            {Ash.Resource.Calculation.Function, 2}},
           {:custom, __MODULE__, :expr_calc, []}
         ]},
      required: true,
      doc: """
      The `module`, `{module, opts}` or `expr(...)` to use for the calculation. Also accepts a function that takes *a single record* and produces the result.
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
    load: [
      type: :any,
      default: [],
      doc: "A load statement to be applied if the calculation is used."
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: "Whether or not the calculation can return nil."
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
          description: nil | String.t(),
          filterable?: boolean,
          load: keyword,
          sortable?: boolean,
          name: atom(),
          public?: boolean,
          type: nil | Ash.Type.t()
        }

  @type ref :: {module(), Keyword.t()} | module()
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

  @callback init(opts :: opts) :: {:ok, opts} | {:error, term}
  @callback describe(opts :: opts) :: String.t()

  @callback calculate(records :: [Ash.Resource.record()], opts :: opts, context :: Context.t()) ::
              {:ok, [term]} | [term] | {:error, term} | :unknown
  @callback expression(opts :: opts, context :: Context.t()) :: any
  @callback load(query :: Ash.Query.t(), opts :: opts, context :: Context.t()) ::
              atom | [atom] | Keyword.t()
  @callback strict_loads?() :: boolean()
  @callback has_expression?() :: boolean()

  @optional_callbacks expression: 2, calculate: 3

  def schema, do: @schema

  def expr_calc(expr) when is_function(expr) do
    {:error,
     "Inline function calculations expect a function with arity 2. Got #{Function.info(expr)[:arity]}"}
  end

  def expr_calc(expr) do
    {:ok, {Ash.Resource.Calculation.Expression, expr: expr}}
  end
end
