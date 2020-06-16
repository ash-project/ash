defmodule Ash.Filter2.Predicate.Eq do
  @moduledoc false
  defstruct [:value]

  use Ash.Filter2.Predicate

  def new(_type, value) do
    {:ok, %__MODULE__{value: value}}
  end

  def compare(%__MODULE__{value: value}, %__MODULE__{value: value}), do: :equal

  def inspect(field, %{value: value}, opts) do
    import Inspect.Algebra

    concat([field, " == ", to_doc(value, opts)])
  end
end
