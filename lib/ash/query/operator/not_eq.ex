defmodule Ash.Query.Operator.NotEq do
  @moduledoc """
  left != right

  In comparison, simplifies to `not(left == right)`
  """
  use Ash.Query.Operator, operator: :!=, predicate?: true

  alias Ash.Query.Not
  alias Ash.Query.Operator.Eq

  def new(%Ref{} = ref, nil) do
    Ash.Query.Operator.new(Ash.Query.Operator.IsNil, ref, false)
  end

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} ->
        {:ok, left, casted}

      _ ->
        {:error,
         Ash.Error.Query.InvalidFilterValue.exception(
           value: right,
           message: "Could not be casted to type #{inspect(type)}",
           context: %__MODULE__{left: left, right: right}
         )}
    end
  end

  def new(left, right) do
    {:known, left != right}
  end

  def evaluate(%{left: left, right: right}) do
    left != right
  end

  def simplify(%__MODULE__{left: left, right: right}) do
    %Not{expression: %Eq{left: left, right: right}}
  end
end
