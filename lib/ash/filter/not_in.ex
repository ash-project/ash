defmodule Ash.Filter.NotIn do
  @moduledoc false
  defstruct [:values]

  alias Ash.Error.Filter.InvalidFilterValue
  alias Ash.Filter.NotEq

  def new(resource, attr_name, attr_type, [value]) do
    NotEq.new(resource, attr_name, attr_type, value)
  end

  def new(_resource, attr_name, attr_type, values) do
    casted =
      values
      |> List.wrap()
      |> Enum.reduce({:ok, []}, fn
        value, {:ok, casted} ->
          case Ash.Type.cast_input(attr_type, value) do
            {:ok, value} ->
              {:ok, [value | casted]}

            :error ->
              {:error,
               InvalidFilterValue.exception(
                 filter: %__MODULE__{values: values},
                 value: value,
                 field: attr_name
               )}
          end

        _, {:error, error} ->
          {:error, error}
      end)

    case casted do
      {:error, error} ->
        {:error, error}

      {:ok, values} ->
        {:ok, %__MODULE__{values: values}}
    end
  end
end

defimpl Inspect, for: Ash.Filter.NotIn do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{values: values}, opts) do
    concat([attr(opts), " not in ", to_doc(values, opts)])
  end
end
