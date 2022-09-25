defmodule Ash.Query.Exists do
  @moduledoc """
  Determines if a given related entity exists.
  """

  defstruct [:path, :expr, at_path: []]

  def new(path, expr, at_path \\ [])

  def new([], expr, _) do
    raise "Cannot construct an exists query with an empty path, at #{inspect(%__MODULE__{path: [], expr: expr})}"
  end

  def new(path, expr, at_path) do
    %__MODULE__{path: path, expr: expr, at_path: at_path}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{path: path, expr: expr, at_path: at_path}, opts) do
      if at_path && at_path != [] do
        concat([
          Enum.join(at_path, "."),
          ".",
          "exists(",
          Enum.join(path, "."),
          ", ",
          to_doc(expr, opts),
          ")"
        ])
      else
        concat(["exists(", Enum.join(path, "."), ", ", to_doc(expr, opts), ")"])
      end
    end
  end
end
