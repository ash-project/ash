# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.Basic do
  @moduledoc false
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
      evaluate_types: :numbers,
      types: [
        [:same, :any],
        [:duration, :integer],
        [:integer, :duration]
      ],
      returns: [:same, :duration, :duration]
    ],
    minus: [
      symbol: :-,
      no_nils: true,
      evaluate_types: :numbers
    ],
    div: [
      symbol: :/,
      no_nils: true,
      evaluate_types: :numbers,
      types: [
        [:float, :float],
        [:decimal, :decimal],
        [:float, :decimal],
        [:decimal, :float],
        [:integer, :integer],
        [:integer, :float],
        [:integer, :decimal],
        [:float, :integer],
        [:decimal, :integer]
      ],
      returns: [:float, :decimal, :decimal, :decimal, :float, :float, :decimal, :float, :decimal]
    ],
    concat: [
      symbol: :<>,
      no_nils: true,
      types: [
        [:string, :string],
        [:ci_string, :ci_string],
        [:string, :ci_string],
        [:ci_string, :string]
      ],
      returns: [:string, :ci_string, :ci_string, :ci_string]
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
          returns: unquote(opts[:returns] || [:same]),
          types: unquote(opts[:types] || [[:same, :any]])

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
          {:known, %Ash.CiString{string: left <> right}}
        end

        defp do_evaluate(:<>, %Ash.CiString{string: left}, right) do
          {:known, %Ash.CiString{string: left <> right}}
        end

        defp do_evaluate(:<>, left, %Ash.CiString{string: right}) do
          {:known, %Ash.CiString{string: left <> right}}
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

        defp do_evaluate(:==, left, right) do
          {:known, Comp.equal?(left, right)}
        end

        defp do_evaluate(op, left, right)
             when Decimal.is_decimal(left) and Decimal.is_decimal(right) do
          case op do
            :+ -> {:known, Decimal.add(to_decimal(left), to_decimal(right))}
            :* -> {:known, Decimal.mult(to_decimal(left), to_decimal(right))}
            :- -> {:known, Decimal.sub(to_decimal(left), to_decimal(right))}
            :/ -> {:known, Decimal.div(to_decimal(left), to_decimal(right))}
          end
        end

        defp do_evaluate(:-, %left_name{} = left, %right_name{} = right) do
          case {left_name, right_name} do
            {Duration, Duration} ->
              {:known, Duration.subtract(left, right)}

            {Date, Duration} ->
              {:known, Date.shift(left, Duration.negate(right))}

            {Date, _} ->
              {:known, Date.diff(left, right)}

            {DateTime, Duration} ->
              {:known, DateTime.shift(left, Duration.negate(right))}

            {DateTime, _} ->
              {:known, DateTime.diff(left, right)}

            {Time, Duration} ->
              {:known, Time.shift(left, Duration.negate(right))}

            {Time, _} ->
              {:known, Time.diff(left, right)}

            {NaiveDateTime, Duration} ->
              {:known, NaiveDateTime.shift(left, Duration.negate(right))}

            {NaiveDateTime, _} ->
              {:known, NaiveDateTime.diff(left, right)}

            _ ->
              :unknown
          end
        end

        defp do_evaluate(:+, %left_name{} = left, %right_name{} = right) do
          case {left_name, right_name} do
            {Duration, Duration} -> {:known, Duration.add(left, right)}
            {Date, Duration} -> {:known, Date.shift(left, right)}
            {Duration, Date} -> {:known, Date.shift(right, left)}
            {DateTime, Duration} -> {:known, DateTime.shift(left, right)}
            {Duration, DateTime} -> {:known, DateTime.shift(right, left)}
            {NaiveDateTime, Duration} -> {:known, NaiveDateTime.shift(left, right)}
            {Duration, NaiveDateTime} -> {:known, NaiveDateTime.shift(right, left)}
            {Time, Duration} -> {:known, Time.shift(left, right)}
            {Duration, Time} -> {:known, Time.shift(right, left)}
            _ -> :unknown
          end
        end

        defp do_evaluate(:*, left, right)
             when is_struct(left, Duration) or is_struct(right, Duration) do
          cond do
            is_struct(left, Duration) and is_integer(right) ->
              {:known, Duration.multiply(left, right)}

            is_integer(left) and is_struct(right, Duration) ->
              {:known, Duration.multiply(right, left)}
          end
        end

        defp do_evaluate(op, left, right)
             when is_struct(left, Duration) and is_struct(right, Duration) do
          case op do
            :+ -> {:known, Duration.add(left, right)}
            :- -> {:known, Duration.subtract(left, right)}
            _ -> :unknown
          end
        end

        defp do_evaluate(:*, left, right)
             when is_struct(left, Duration) or is_struct(right, Duration) do
          cond do
            is_struct(left, Duration) and is_integer(right) ->
              {:known, Duration.multiply(left, right)}

            is_integer(left) and is_struct(right, Duration) ->
              {:known, Duration.multiply(right, left)}
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
