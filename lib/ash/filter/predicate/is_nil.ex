defmodule Ash.Filter.Predicate.IsNil do
  @moduledoc "A predicate for checking if a value is nil"
  defstruct [:field, :nil?]

  use Ash.Filter.Predicate

  alias Ash.Error.Query.InvalidFilterValue
  alias Ash.Filter.Predicate

  def new(_resource, attribute, nil?) when is_boolean(nil?) do
    {:ok, %__MODULE__{field: attribute.name, nil?: nil?}}
  end

  def new(_resource, attribute, value) do
    {:error,
     InvalidFilterValue.exception(
       value: value,
       context: %__MODULE__{field: attribute.name, nil?: value},
       message: "Expected a boolean for `is_nil` filter"
     )}
  end

  def match?(%{nil?: true}, value, _), do: is_nil(value)
  def match?(%{nil?: false}, value, _), do: not is_nil(value)

  def compare(%__MODULE__{nil?: nil?}, %__MODULE__{nil?: nil?}), do: :mutually_inclusive
  def compare(_, _), do: :unknown

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{nil?: true} = predicate, opts) do
      concat([
        "is_nil(",
        Predicate.add_inspect_path(opts, predicate.field),
        ")"
      ])
    end

    def inspect(%{nil?: false} = predicate, opts) do
      concat([
        "not(is_nil(",
        Predicate.add_inspect_path(opts, predicate.field),
        "))"
      ])
    end
  end
end
