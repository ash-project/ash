defmodule Ash.Filter.Predicate.In do
  @moduledoc false
  defstruct [:field, :values]

  alias Ash.Error.Filter.InvalidFilterValue
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

  # def compare(%__MODULE__{values: values}, %__MODULE__{values: values}), do: :equal
  # def compare(%__MODULE__{values: values}, %__MODULE__{values: other_values}) when
  #   # MapSet.new(values)
  # end

  # def compare(%__MODULE__{value: value}, %NotEq{value: other_value}) when value != other_value,
  #   do: :inclusive
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
