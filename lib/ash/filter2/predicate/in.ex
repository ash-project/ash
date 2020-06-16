defmodule Ash.Filter2.Predicate.In do
  @moduledoc false
  defstruct [:values]

  alias Ash.Filter2.Predicate.Eq

  # alias Ash.Filter2.Predicate.NotEq
  # alias Ash.Filter2.Predicate.NotIn

  use Ash.Filter2.Predicate

  def new(_type, []), do: {:ok, %__MODULE__{values: MapSet.new([])}}

  def new(type, [value]), do: {:ok, Eq.new(type, value)}

  def new(type, values) when is_list(values) do
    values = MapSet.new(values)

    case MapSet.size(values) do
      1 -> {:ok, Eq.new(type, Enum.at(values, 0))}
      _ -> {:ok, %__MODULE__{values: values}}
    end
  end

  def new(_type, values) do
    {:error, "Invalid filter value provided for `in`: #{inspect(values)}"}
  end

  # def compare(%__MODULE__{values: values}, %__MODULE__{values: values}), do: :equal
  # def compare(%__MODULE__{values: values}, %__MODULE__{values: other_values}) when
  #   # MapSet.new(values)
  # end

  # def compare(%__MODULE__{value: value}, %NotEq{value: other_value}) when value != other_value,
  #   do: :inclusive

  def inspect(field, %{values: values}, opts) do
    import Inspect.Algebra

    concat([field, " in ", to_doc(Enum.to_list(values), opts)])
  end
end

# defmodule Ash.Filter.In do
#   @moduledoc false
#   defstruct [:values]

#   alias Ash.Error.Filter.InvalidFilterValue
#   alias Ash.Filter.Eq

#   def new(resource, attr_name, attr_type, [value]) do
#     Eq.new(resource, attr_name, attr_type, value)
#   end

#   def new(_resource, attr_name, attr_type, values) do
#     casted =
#       values
#       |> List.wrap()
#       |> Enum.reduce({:ok, []}, fn
#         value, {:ok, casted} ->
#           case Ash.Type.cast_input(attr_type, value) do
#             {:ok, value} ->
#               {:ok, [value | casted]}

#             :error ->
#               {:error,
#                InvalidFilterValue.exception(
#                  filter: %__MODULE__{values: values},
#                  value: value,
#                  field: attr_name
#                )}
#           end

#         _, {:error, error} ->
#           {:error, error}
#       end)

#     case casted do
#       {:error, error} ->
#         {:error, error}

#       {:ok, values} ->
#         {:ok, %__MODULE__{values: values}}
#     end
#     |> case do
#       {:ok, %{values: values} = in_operator} -> {:ok, %{in_operator | values: Enum.uniq(values)}}
#       {:error, error} -> {:error, error}
#     end
#   end
# end

# defimpl Inspect, for: Ash.Filter.In do
#   import Inspect.Algebra
#   import Ash.Filter.InspectHelpers

#   def inspect(%{values: values}, opts) do
#     concat([attr(opts), " in ", to_doc(values, opts)])
#   end
# end
