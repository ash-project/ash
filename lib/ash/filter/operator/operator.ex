defmodule Ash.Filter.Operator do
  @callback new(term, term) :: {:ok, term, term} | {:known, boolean} | {:error, Ash.error()}
  @callback to_string(struct, Inspect.Opts.t()) :: term

  def new(mod, left, right) do
    case mod.new(left, right) do
      {:ok, left, right} -> {:ok, struct(mod, left: left, right: right)}
      {:ok, %_{} = op} -> {:ok, op}
      {:known, result} -> {:ok, result}
      {:error, error} -> {:error, error}
    end
  end

  def match?(operator, left, right) do
    operator.match?(left, right)
  end

  defmacro __using__(opts) do
    unless opts[:operator] do
      raise "Operator is required!"
    end

    quote do
      defstruct [
        :left,
        :right,
        operator: unquote(opts[:operator]),
        embedded: false,
        __operator__?: true,
        __predicate__?: true
      ]

      use Ash.Filter.Predicate

      alias Ash.Filter.Ref

      def operator, do: unquote(opts[:operator])

      import Inspect.Algebra

      def to_string(%{left: left, right: right, operator: operator}, opts) do
        concat([
          to_doc(left, opts),
          " ",
          to_string(operator),
          " ",
          to_doc(right, opts)
        ])
      end

      defoverridable to_string: 2

      defimpl Inspect do
        def inspect(%mod{} = op, opts) do
          mod.to_string(op, opts)
        end
      end
    end
  end
end
