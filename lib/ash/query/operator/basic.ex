defmodule Ash.Query.Operator.Basic do
  @operators [
    plus: [
      symbol: :+,
      no_nils: true
    ],
    times: [
      symbol: :*,
      no_nils: true
    ],
    minus: [
      symbol: :-,
      no_nils: true
    ],
    div: [
      symbol: :/,
      no_nils: true
    ],
    concat: [
      symbol: :<>,
      no_nils: true
    ],
    or: [
      symbol: :||
    ],
    and: [
      symbol: :&&
    ]
  ]

  Module.register_attribute(__MODULE__, :operator_modules, accumulate: true)

  for {name, opts} <- @operators do
    mod = Module.concat([__MODULE__, String.capitalize(to_string(name))])
    @operator_modules mod

    Module.create(
      mod,
      quote generated: true do
        @moduledoc false

        require Decimal

        use Ash.Query.Operator,
          operator: unquote(opts[:symbol]),
          name: unquote(name),
          predicate?: false,
          types: [:same, :any]

        if unquote(opts[:no_nils]) do
          def evaluate(%{left: left, right: right}) do
            if is_nil(left) || is_nil(right) do
              {:known, nil}
            else
              # delegate to function to avoid dialyzer warning
              # that this can only ever be one value (for each module we define)
              do_evaluate(unquote(opts[:symbol]), left, right)
            end
          end
        else
          def evaluate(%{left: left, right: right}) do
            # delegate to function to avoid dialyzer warning
            # that this can only ever be one value (for each module we define)
            do_evaluate(unquote(opts[:symbol]), left, right)
          end
        end

        defp do_evaluate(:<>, %Ash.CiString{string: left}, %Ash.CiString{string: right}) do
          %Ash.CiString{string: left <> right}
        end

        defp do_evaluate(:<>, %Ash.CiString{string: left}, right) do
          %Ash.CiString{string: left <> right}
        end

        defp do_evaluate(:<>, left, %Ash.CiString{string: right}) do
          %Ash.CiString{string: left <> right}
        end

        defp do_evaluate(:<>, left, right) do
          {:known, to_string(left) <> to_string(right)}
        end

        defp do_evaluate(:<>, left, right) do
          {:known, to_string(left) <> to_string(right)}
        end

        defp do_evaluate(:||, left, right) do
          {:known, left || right}
        end

        defp do_evaluate(:&&, left, right) do
          {:known, left && right}
        end

        defp do_evaluate(:<, left, right) do
          {:known, Comp.less_than?(left, right)}
        end

        defp do_evaluate(:<=, left, right) do
          {:known, Comp.less_or_equal?(left, right)}
        end

        defp do_evaluate(:>, left, right) do
          {:known, Comp.greater_than?(left, right)}
        end

        defp do_evaluate(:>=, left, right) do
          {:known, Comp.greater_or_equal?(left, right)}
        end

        defp do_evaluate(op, left, right) do
          if Decimal.is_decimal(left) || Decimal.is_decimal(right) do
            case op do
              :+ -> {:known, Decimal.add(to_decimal(left), to_decimal(right))}
              :* -> {:known, Decimal.mult(to_decimal(left), to_decimal(right))}
              :- -> {:known, Decimal.sub(to_decimal(left), to_decimal(right))}
              :/ -> {:known, Decimal.div(to_decimal(left), to_decimal(right))}
            end
          else
            {:known, apply(Kernel, unquote(opts[:symbol]), [left, right])}
          end
        end

        defp to_decimal(value) when is_float(value) do
          Decimal.from_float(value)
        end

        defp to_decimal(value) do
          Decimal.new(value)
        end
      end,
      Macro.Env.location(__ENV__)
    )
  end

  def operator_modules do
    @operator_modules
  end
end
