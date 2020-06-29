defmodule Ash.Filter.Predicate.In do
  @moduledoc "A predicate for a value being in a list of provided values"
  defstruct [:field, :values]

  use Ash.Filter.Predicate

  alias Ash.Error.Filter.InvalidFilterValue
  alias Ash.Filter.Expression
  alias Ash.Filter.Predicate
  alias Ash.Filter.Predicate.Eq

  def new(_resource, attribute, []),
    do: {:ok, %__MODULE__{field: attribute.name, values: MapSet.new([])}}

  def new(resource, attribute, [value]), do: Eq.new(resource, attribute, value)

  def new(_resource, attribute, values) when is_list(values) do
    Enum.reduce_while(values, {:ok, %__MODULE__{field: attribute.name, values: []}}, fn value,
                                                                                        {:ok,
                                                                                         predicate} ->
      case Ash.Type.cast_input(attribute.type, value) do
        {:ok, casted} ->
          {:cont, {:ok, %{predicate | values: [casted | predicate.values]}}}

        :error ->
          {:halt,
           {:error,
            InvalidFilterValue.exception(
              filter: %__MODULE__{field: attribute.name, values: values},
              value: value,
              field: attribute.name
            )}}
      end
    end)
  end

  def new(_resource, attribute, values) do
    {:error,
     InvalidFilterValue.exception(
       filter: %__MODULE__{field: attribute.name, values: values},
       value: values,
       field: attribute.name
     )}
  end

  def compare(%__MODULE__{} = left, %__MODULE__{} = right) do
    {:simplify, in_to_or_equals(left), in_to_or_equals(right)}
  end

  def compare(%__MODULE__{} = in_expr, _) do
    {:simplify, in_to_or_equals(in_expr)}
  end

  def compare(_, _), do: :unknown

  defp in_to_or_equals(%{field: field, values: values}) do
    Enum.reduce(values, nil, fn value, expression ->
      Expression.new(:or, expression, %Eq{field: field, value: value})
    end)
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(predicate, opts) do
      concat([
        Predicate.add_inspect_path(opts, predicate.field),
        " in ",
        to_doc(predicate.values, opts)
      ])
    end
  end
end
