defmodule Ash.Query.Calculation do
  @moduledoc "Represents a calculated attribute requested on a query"

  defstruct [
    :name,
    :module,
    :opts,
    :load,
    :type,
    :constraints,
    :calc_name,
    context: %{},
    required_loads: [],
    select: [],
    filterable?: true,
    sortable?: true,
    sensitive?: false
  ]

  @type t :: %__MODULE__{}

  @opt_schema [
    arguments: [
      type: :map,
      doc: "Arguments to pass to the calculation",
      default: %{}
    ],
    filterable?: [
      type: :boolean,
      doc: "Whether or not this calculation can be filtered on",
      default: true
    ],
    sortable?: [
      type: :boolean,
      doc: "Whether or not this calculation can be sorted on",
      default: true
    ],
    sensitive?: [
      type: :boolean,
      doc: "Whether or not references to this calculation will be considered sensitive",
      default: false
    ],
    load: [
      type: :any,
      doc: "Loads that are required for the calculation."
    ],
    source_context: [
      type: :map,
      doc: "Context from the source query or changeset.",
      default: %{}
    ]
  ]

  @doc """
  Creates a new query calculation.

  ## Options

  #{Spark.Options.docs(@opt_schema)}
  """
  def new(
        name,
        module,
        calc_opts,
        type,
        constraints,
        opts \\ []
      ) do
    with {:ok, opts} <- Spark.Options.validate(opts, @opt_schema),
         {:ok, calc_opts} <- module.init(calc_opts) do
      context = %Ash.Resource.Calculation.Context{
        arguments: opts[:arguments],
        type: type,
        constraints: constraints,
        source_context: opts[:source_context] || %{}
      }

      {:ok,
       %__MODULE__{
         name: name,
         module: module,
         type: type,
         opts: calc_opts,
         calc_name: name,
         constraints: constraints,
         context: context,
         required_loads: opts[:load],
         filterable?: opts[:filterable?],
         sortable?: opts[:sortable?],
         sensitive?: opts[:sensitive?]
       }}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{module: module, opts: calculation_opts, context: context}, _opts) do
      if context.arguments == %{} do
        module.describe(calculation_opts)
      else
        concat([module.describe(calculation_opts), " - ", inspect(context.arguments)])
      end
    end
  end
end
