defmodule Ash.Filter.Predicate.In do
  @moduledoc false
  defstruct [:field, :values]

  alias Ash.Error.Filter.InvalidFilterValue
  alias Ash.Filter.Expression
  alias Ash.Filter.Predicate.Eq

  use Ash.Filter.Predicate

  def new(_resource, attribute, []),
    do: {:ok, %__MODULE__{field: attribute.name, values: MapSet.new([])}}

  def new(resource, attribute, [value]), do: {:ok, Eq.new(resource, attribute, value)}

  def new(_resource, attribute, values) when is_list(values) do
    Enum.reduce_while(values, {:ok, %__MODULE__{field: attribute.name, values: []}}, fn value,
                                                                                        predicate ->
      case Ash.Type.cast_input(attribute.type, value) do
        {:ok, casted} ->
          {:cont, {:ok, %{predicate | values: [casted | predicate.values]}}}

        :error ->
          {:error,
           InvalidFilterValue.exception(
             filter: %__MODULE__{field: attribute.name, values: values},
             value: value,
             field: attribute.name
           )}
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

  def compare(%__MODULE__{field: field, values: values}, %__MODULE__{values: other_values}) do
    left = MapSet.new(values)
    right = MapSet.new(other_values)

    common_members = MapSet.intersection(left, right)

    if Enum.empty?(common_members) do
      :exclusive
    else
      different_members = MapSet.difference(left, right)

      if Enum.empty?(different_members) do
        :mutually_inclusive
      else
        {:simplify,
         Expression.new(
           :or,
           %__MODULE__{field: field, values: MapSet.to_list(different_members)},
           %__MODULE__{field: field, values: MapSet.to_list(common_members)}
         )}
      end
    end
  end

  def compare(
        %__MODULE__{values: values} = in_predicate,
        %Eq{value: value} = equals
      ) do
    if value in values do
      {:simplify, Expression.new(:or, equals, %{in_predicate | values: values -- [value]})}
    else
      :mutually_exclusive
    end
  end

  def compare(_, _), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(predicate, opts) do
      concat([
        Ash.Filter.Predicate.add_inspect_path(opts, predicate.field),
        " in ",
        to_doc(predicate.values, opts)
      ])
    end
  end
end
