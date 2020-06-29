defmodule Ash.Filter.Predicate.LessThan do
  @moduledoc "A predicate for a value being greater than the provided value"
  defstruct [:field, :value, :type]

  alias Ash.Filter.Predicate.Eq

  alias Ash.Error.Filter.InvalidFilterValue

  use Ash.Filter.Predicate

  def new(_resource, attribute, value) do
    case Ash.Type.cast_input(attribute.type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{field: attribute.name, value: value}}

      :error ->
        {:error,
         InvalidFilterValue.exception(
           filter: %__MODULE__{field: attribute.name, value: value},
           value: value,
           field: attribute.name
         )}
    end
  end

  def compare(%__MODULE__{value: value}, %__MODULE__{value: value}), do: :mutually_inclusive

  def compare(%__MODULE__{value: value}, %__MODULE__{value: other_value})
      when value < other_value do
    :right_includes_left
  end

  def compare(%__MODULE__{value: value}, %__MODULE__{value: other_value})
      when value > other_value do
    :left_includes_right
  end

  def compare(%__MODULE__{value: value}, %Eq{value: eq_value}) when eq_value < value do
    :left_includes_right
  end

  def compare(%__MODULE__{}, %Eq{}) do
    :mutually_exclusive
  end

  def compare(_, _), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra
    alias Ash.Filter.Predicate

    def inspect(predicate, opts) do
      concat([
        Predicate.add_inspect_path(opts, predicate.field),
        " < ",
        to_doc(predicate.value, opts)
      ])
    end
  end
end
