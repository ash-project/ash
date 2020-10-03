defmodule Ash.Filter.Operator do
  @callback new(term, term) :: {:ok, term, term} | {:known, boolean} | {:error, Ash.error()}
  @callback prepare_for_inspect(term, term) :: {term, term}

  def new(mod, left, right) do
    case mod.new(left, right) do
      {:ok, left, right} -> {:ok, struct(mod, left: left, right: right)}
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

      def prepare_for_inspect(left, right), do: {left, right}

      defoverridable prepare_for_inspect: 2

      defimpl Inspect do
        import Inspect.Algebra

        def inspect(%mod{left: left, right: right, operator: operator}, opts) do
          {left, right} = mod.prepare_for_inspect(left, right)

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
