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
    async?: false,
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
    async?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this calculation should be run asynchronously"
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
    calc_name: [
      type: :any,
      hide: true,
      doc: "A resource calculation this calculation maps to."
    ],
    source_context: [
      type: :map,
      doc: "Context from the source query or changeset.",
      default: %{}
    ]
  ]

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

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
    with {:ok, opts} <- Opts.validate(opts),
         {:ok, calc_opts} <- module.init(calc_opts) do
      context = %Ash.Resource.Calculation.Context{
        arguments: opts.arguments,
        type: type,
        constraints: constraints,
        source_context: opts.source_context
      }

      calc_name =
        if :calc_name in opts.__set__ do
          opts.calc_name
        end

      {:ok,
       %__MODULE__{
         name: name,
         module: module,
         type: type,
         opts: calc_opts,
         calc_name: calc_name,
         constraints: constraints,
         context: context,
         async?: opts.async?,
         required_loads: opts.load,
         filterable?: opts.filterable?,
         sortable?: opts.sortable?,
         sensitive?: opts.sensitive?
       }}
    end
  end

  @from_resource_schema [
    args: [
      type: :map,
      doc: "Arguments to pass to the calculation",
      default: %{}
    ],
    source_context: [
      type: :map,
      doc: "Context from the source query or changeset.",
      default: %{}
    ]
  ]

  from_resource_schema = @from_resource_schema

  defmodule FromResourceOpts do
    @moduledoc false
    use Spark.Options.Validator, schema: from_resource_schema
  end

  @doc """
  Creates a new query calculation from a resource calculation, raising any errors.

  See `from_resource_calculation/3` for more.
  """
  def from_resource_calculation!(
        resource,
        name,
        opts \\ []
      ) do
    case from_resource_calculation(resource, name, opts) do
      {:ok, calculation} ->
        calculation

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  @doc """
  Creates a new query calculation from a resource calculation.

  ## Options

  #{Spark.Options.docs(@from_resource_schema)}
  """
  def from_resource_calculation(
        resource,
        name,
        opts \\ []
      ) do
    {name, resource_calculation} =
      case name do
        %Ash.Resource.Calculation{} = calc ->
          {calc.name, calc}

        name ->
          {name,
           Ash.Resource.Info.calculation(resource, name) ||
             raise(
               ArgumentError,
               "No calculation called #{inspect(name)} found on #{inspect(resource)}"
             )}
      end

    %{calculation: {module, calc_opts}} = resource_calculation

    with {:ok, opts} <- FromResourceOpts.validate(opts),
         {:ok, args} <-
           Ash.Query.validate_calculation_arguments(resource_calculation, opts.args) do
      new(
        name,
        module,
        calc_opts,
        resource_calculation.type,
        resource_calculation.constraints,
        arguments: args,
        async?: resource_calculation.async?,
        filterable?: resource_calculation.filterable?,
        sortable?: resource_calculation.sortable?,
        sensitive?: resource_calculation.sensitive?,
        load: resource_calculation.load,
        source_context: opts.source_context,
        calc_name: resource_calculation.name
      )
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{context: context, calc_name: calc_name}, inspect_opts)
        when not is_nil(calc_name) do
      calc_name =
        if is_atom(calc_name) or is_binary(calc_name) do
          to_string(calc_name)
        else
          to_doc(calc_name, inspect_opts)
        end

      if context.arguments == %{} do
        calc_name
      else
        concat([
          calc_name,
          "(",
          to_doc(Map.to_list(context.arguments), inspect_opts),
          ")"
        ])
      end
    end

    def inspect(%{module: module, opts: calculation_opts, context: context}, _opts) do
      if context.arguments == %{} do
        module.describe(calculation_opts)
      else
        concat([module.describe(calculation_opts), " - ", inspect(context.arguments)])
      end
    end
  end
end
