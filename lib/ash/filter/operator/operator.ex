defmodule Ash.Filter.Operator do
  @callback new(term, term) :: {:ok, term, term} | {:known, boolean} | {:error, Ash.error()}

  def new(mod, left, right) do
    case mod.new(left, right) do
      {:ok, left, right} -> {:ok, struct(mod, left: left, right: right)}
      {:known, result} -> {:ok, result}
      {:error, error} -> {:error, error}
    end
  end

  defmacro __using__(opts) do
    unless opts[:operator] do
      raise "Operator is required!"
    end

    quote do
      defstruct [:left, :right, embedded?: false] ++ unquote(opts[:fields] || [])

      alias Ash.Filter.Ref

      defimpl Inspect do
        import Inspect.Algebra

        def inspect(%{left: left, right: right, operator: operator}, opts) do
          concat([
            to_doc(left, opts),
            " ",
            to_string(unquote(opts[:operator])),
            " ",
            to_doc(right, opts)
          ])
        end
      end
    end
  end
end
