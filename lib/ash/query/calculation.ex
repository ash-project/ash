defmodule Ash.Query.Calculation do
  @moduledoc "Represents a calculated attribute requested on a query"

  defstruct [:name, :module, :opts, :load, :type, context: %{}, select: [], sequence: 0]

  @type t :: %__MODULE__{}

  def new(name, module, opts, type, context \\ %{}) do
    case module.init(opts) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           module: module,
           type: type,
           opts: opts,
           context: context
         }}

      {:error, error} ->
        {:error, error}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{module: module, opts: calculation_opts, context: context}, _opts) do
      if context == %{} do
        module.describe(calculation_opts)
      else
        concat([module.describe(calculation_opts), " - ", inspect(context)])
      end
    end
  end
end
