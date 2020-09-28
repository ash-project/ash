defmodule Ash.Filter.Operator.Eq do
  use Ash.Filter.Operator, operator: :==

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} -> {:ok, left, casted}
      :error -> {:ok, left, right}
    end
  end

  def new(left, right) do
    {:known, left == right}
  end

  def match?(left, right) do
    left == right
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: same_value}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: same_value
      }) do
    :mutually_inclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref}, %__MODULE__{left: %Ref{} = same_ref}) do
    :left_excludes_right
  end

  def compare(_, _), do: :unknown
end
