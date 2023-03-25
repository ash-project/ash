defmodule Ash.Query.Calculation do
  @moduledoc "Represents a calculated attribute requested on a query"

  defstruct [
    :name,
    :module,
    :opts,
    :load,
    :type,
    :constraints,
    context: %{},
    required_loads: [],
    select: [],
    sequence: 0,
    filterable?: true
  ]

  @type t :: %__MODULE__{}

  def new(name, module, opts, type, context \\ %{}, filterable? \\ true, required_loads \\ []) do
    {type, constraints} =
      case type do
        {:array, type} ->
          {{:array, type}, []}

        {type, constraints} ->
          {type, constraints}

        other ->
          {other, []}
      end

    context = Map.put(context, :ash, %{type: type, constraints: constraints})

    case module.init(opts) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           module: module,
           type: type,
           opts: opts,
           constraints: constraints,
           context: context,
           required_loads: required_loads,
           filterable?: filterable?
         }}

      {:error, error} ->
        {:error, error}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{module: module, opts: calculation_opts, context: context}, _opts) do
      context =
        Map.drop(context, [
          :actor,
          :authorize?,
          :filter_requests,
          :initial_limit,
          :initial_offset,
          :context,
          :tenant,
          :tracer,
          :ash
        ])

      if context == %{} do
        module.describe(calculation_opts)
      else
        concat([module.describe(calculation_opts), " - ", inspect(context)])
      end
    end
  end
end
