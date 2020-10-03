defmodule Ash.Filter.Operator.Eq do
  use Ash.Filter.Operator, operator: :==

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} ->
        {:ok, left, casted}

      _ ->
        {:error,
         Ash.Error.Query.InvalidFilterValue.exception(
           value: right,
           context: %__MODULE__{left: left, right: right}
         )}
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
    :mutually_exclusive
  end

  def compare(_, _), do: :unknown
end
