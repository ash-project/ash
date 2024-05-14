defmodule Ash.Query.Operator.Basic do
  require Decimal

  @operators [
    plus: [
      symbol: :+,
      no_nils: true,
      evaluate_types: :numbers
    ],
    times: [
      symbol: :*,
      no_nils: true,
      evaluate_types: :numbers
    ],
    minus: [
      symbol: :-,
      no_nils: true,
      evaluate_types: :numbers
    ],
    div: [
      symbol: :/,
      no_nils: true,
      evaluate_types: :numbers
    ],
    concat: [
      symbol: :<>,
      no_nils: true,
      types: [[:string, :string]]
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
          types: unquote(opts[:types] || [:same, :any])

        if unquote(opts[:no_nils]) do
          @impl Ash.Query.Operator
          def evaluate(%{left: left, right: right}) do
            if is_nil(left) || is_nil(right) do
              {:known, nil}
            else
              # delegate to function to avoid dialyzer warning
              # that this can only ever be one value (for each module we define)
              do_evaluate(unquote(opts[:symbol]), left, right)
            end
          end

          @impl Ash.Query.Operator
          def evaluate_nil_inputs?, do: false
        else
          @impl Ash.Query.Operator
          def evaluate(%{left: left, right: right}) do
            # delegate to function to avoid dialyzer warning
            # that this can only ever be one value (for each module we define)
            do_evaluate(unquote(opts[:symbol]), left, right)
          end

          @impl Ash.Query.Operator
          def evaluate_nil_inputs?, do: true
        end

        def partial_evaluate(%{left: left, right: right} = pred) do
          case unquote(opts[:symbol]) do
            :|| ->
              case left do
                false ->
                  {:ok, right}

                nil ->
                  {:ok, right}

                other ->
                  if Ash.Expr.expr?(other) do
                    {:ok, pred}
                  else
                    {:ok, other}
                  end
              end

            :&& ->
              case left do
                false ->
                  {:ok, false}

                nil ->
                  {:ok, nil}

                other ->
                  if Ash.Expr.expr?(other) do
                    {:ok, pred}
                  else
                    {:ok, other}
                  end
              end

            _other ->
              {:ok, pred}
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

        defp do_evaluate(op, left, right)
             when Decimal.is_decimal(left) or Decimal.is_decimal(right) do
          case op do
            :+ -> {:known, Decimal.add(to_decimal(left), to_decimal(right))}
            :* -> {:known, Decimal.mult(to_decimal(left), to_decimal(right))}
            :- -> {:known, Decimal.sub(to_decimal(left), to_decimal(right))}
            :/ -> {:known, Decimal.div(to_decimal(left), to_decimal(right))}
          end
        end

        if unquote(opts[:evaluate_types]) == :numbers do
          defp do_evaluate(op, left, right)
               when not ((is_integer(left) or is_float(left)) and
                           (is_integer(right) or is_float(right))) do
            :unknown
          end

          defp do_evaluate(op, left, right) do
            {:known, apply(Kernel, unquote(opts[:symbol]), [left, right])}
          end
        else
          defp do_evaluate(op, left, right) do
            {:known, apply(Kernel, unquote(opts[:symbol]), [left, right])}
          end
        end

        def can_return_nil?(%{operator: :||, right: right}) do
          Ash.Expr.can_return_nil?(right)
        end

        def can_return_nil?(%{left: left, right: right} = op) do
          Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)
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
