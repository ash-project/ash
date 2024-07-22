defmodule Ash.Query.Parent do
  @moduledoc """
  Used to access values from the "source" of a given expression.

  This is used in cases where expressions are given for some relationship path, for example:any()

  ```elixir
   has_many :foo, Foo do
     filter expr(priority == :foo and type == parent(foo_type))
   end
  ```

  This is supported on a case by case basis by a given data layer and in specific usages.
  """

  defstruct [:expr]

  def new(expr) do
    %__MODULE__{expr: expr}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{expr: expr}, opts) do
      concat([
        "parent(",
        to_doc(expr, opts),
        ")"
      ])
    end
  end
end
