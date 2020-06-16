defmodule Ash.Filter2.Not do
  defstruct [:expression]

  alias Ash.Filter2.Expression

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
