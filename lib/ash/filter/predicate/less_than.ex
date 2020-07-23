defmodule Ash.Filter.Predicate.LessThan do
  @moduledoc "A predicate for a value being greater than the provided value"
  defstruct [:field, :value, :type]

  alias Ash.Filter.Predicate.Eq

  alias Ash.Error.Query.InvalidFilterValue

  use Ash.Filter.Predicate

  def new(_resource, attribute, value) do
    case Ash.Type.cast_input(attribute.type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{field: attribute.name, value: value}}

      _ ->
        {:error,
         InvalidFilterValue.exception(
           value: value,
           context: %__MODULE__{field: attribute.name, value: value},
           message: "Could not be casted type type #{inspect(attribute.type)}"
         )}
    end
  end

  def match?(%{value: predicate_value}, value, _) do
    value > predicate_value
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
