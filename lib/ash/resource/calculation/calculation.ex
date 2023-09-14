defmodule Ash.Resource.Calculation do
  @moduledoc "Represents a named calculation on a resource"

  defstruct allow_nil?: true,
            arguments: [],
            calculation: nil,
            constraints: [],
            description: nil,
            filterable?: true,
            load: [],
            name: nil,
            private?: false,
            select: [],
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
           {:spark_function_behaviour, Ash.Calculation, {Ash.Calculation.Function, 2}},
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
    private?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the calculation will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql See the [security guide](/documentation/topics/security.md) for more.
      """
    ],
    select: [
      type: {:list, :atom},
      default: [],
      doc: "A list of fields to ensure selected if the calculation is used."
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
    ]
  ]

  @type t :: %__MODULE__{
          allow_nil?: boolean,
          arguments: [__MODULE__.Argument.t()],
          calculation: module | {module, keyword},
          constraints: keyword,
          description: nil | String.t(),
          filterable?: boolean,
          load: keyword,
          name: atom(),
          private?: boolean,
          select: keyword,
          type: nil | Ash.Type.t()
        }

  @type ref :: {module(), Keyword.t()} | module()

  def schema, do: @schema

  def expr_calc(expr) when is_function(expr) do
    {:error,
     "Inline function calculations expect a function with arity 2. Got #{Function.info(expr)[:arity]}"}
  end

  def expr_calc(expr) do
    {:ok, {Ash.Resource.Calculation.Expression, expr: expr}}
  end
end
