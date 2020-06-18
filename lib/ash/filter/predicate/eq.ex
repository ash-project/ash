defmodule Ash.Filter.Predicate.Eq do
  @moduledoc false
  defstruct [:field, :value]

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
  def compare(%__MODULE__{value: _}, %__MODULE__{value: _}), do: :mutually_exclusive

  def compare(_, _), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(predicate, opts) do
      concat([
        Ash.Filter.Predicate.add_inspect_path(opts, predicate.field),
        " == ",
        to_doc(predicate.value, opts)
      ])
    end
  end
end
