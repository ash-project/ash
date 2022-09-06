defmodule Ash.Query.Exists do
  @moduledoc """
  Determines if a given related entity exists.
  """

  defstruct [:path, :expr]

  def new([], expr) do
    raise "Cannot construct an exists query with an empty path, at #{inspect(%__MODULE__{path: [], expr: expr})}"
  end

  def new(path, expr) do
    %__MODULE__{path: path, expr: expr}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{path: path, expr: expr}, opts) do
      concat(["exists(", Enum.join(path, "."), ", ", to_doc(expr, opts), ")"])
    end
  end
end
