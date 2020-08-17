defmodule Ash.Filter.Predicate.Eq do
  @moduledoc "A predicate for strict value equality"
  defstruct [:field, :value]

  use Ash.Filter.Predicate

  alias Ash.Error.Query.InvalidFilterValue
  alias Ash.Filter.Predicate

  def new(resource, attribute, nil) do
    Predicate.IsNil.new(resource, attribute, true)
  end

  def new(_resource, attribute, value) do
    case Ash.Type.cast_input(attribute.type, value) do
      {:ok, value} ->
        {:ok, %__MODULE__{field: attribute.name, value: value}}

      _ ->
        {:error,
         InvalidFilterValue.exception(
           value: value,
           context: %__MODULE__{field: attribute.name, value: value},
           message: "Could not be casted to type #{inspect(attribute.type)}"
         )}
    end
  end

  def match?(%{value: predicate_value}, value, type) do
    type.equal?(value, predicate_value)
  end

  def compare(%__MODULE__{value: value}, %__MODULE__{value: value}), do: :mutually_inclusive
  def compare(%__MODULE__{value: _}, %__MODULE__{value: _}), do: :mutually_exclusive

  def compare(_, _), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(predicate, opts) do
      concat([
        Predicate.add_inspect_path(opts, predicate.field),
        " == ",
        to_doc(predicate.value, opts)
      ])
    end
  end
end
