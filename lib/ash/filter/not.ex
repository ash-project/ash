defmodule Ash.Filter.Not do
  @moduledoc "Represents the negation of the contained expression"
  defstruct [:expression]

  alias Ash.Filter.Expression

  def new(nil), do: nil

  def new(expression) do
    %__MODULE__{expression: expression}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{expression: expression}, opts) do
      case expression do
        %Expression{op: :and} ->
          concat(["not ", "(", to_doc(expression, opts), ")"])

        _ ->
          concat(["not ", to_doc(expression, opts)])
      end
    end
  end
end
