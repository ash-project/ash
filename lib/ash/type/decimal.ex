# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Decimal do
  @constraints [
    precision: [
      type: {:or, [{:in, [:arbitrary]}, :pos_integer]},
      default: :arbitrary,
      doc: "Enforces a maximum number of significant digits. Set to :arbitrary for no limit."
    ],
    scale: [
      type: {:or, [{:in, [:arbitrary]}, :non_neg_integer]},
      default: :arbitrary,
      doc: "Enforces a maximum number of decimal places. Set to :arbitrary for no limit."
    ],
    max: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a maximum on the value"
    ],
    min: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a minimum on the value"
    ],
    greater_than: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a minimum on the value (exclusive)"
    ],
    less_than: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a maximum on the value (exclusive)"
    ]
  ]

  import Ash.Expr
  import Ash.Gettext

  @moduledoc """
  Represents a decimal.

  A builtin type that can be referenced via `:decimal`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  require Decimal
  use Ash.Type

  @impl true
  def generator(constraints) do
    max =
      constraints[:less_than]
      |> nudge(:down)
      |> minimum(constraints[:max])

    min =
      constraints[:greater_than]
      |> nudge(:up)
      |> maximum(constraints[:min])

    StreamData.float(max: max, min: min)
    |> StreamData.map(fn float ->
      float
      |> Decimal.from_float()
      |> trunc_scale(constraints[:scale])
      |> trunc_precision(constraints[:precision])
    end)
  end

  @impl true
  def storage_type(_), do: :decimal

  @impl true
  def constraints, do: @constraints

  @impl true
  def init(constraints) do
    {precision, constraints} = Keyword.pop(constraints, :precision)
    {scale, constraints} = Keyword.pop(constraints, :scale)
    precision = precision || :arbitrary
    scale = scale || :arbitrary
    {:ok, [{:precision, precision}, {:scale, scale} | constraints]}
  end

  @impl true
  def matches_type?(%Decimal{}, _), do: true
  def matches_type?(_, _), do: false

  @doc false
  def decimal(value) do
    case cast_input(value, []) do
      {:ok, decimal} ->
        {:ok, decimal}

      :error ->
        {:error, "cannot be casted to decimal"}
    end
  end

  @impl true
  def cast_atomic(expr, constraints) do
    cond do
      constraints[:precision] && constraints[:precision] != :arbitrary ->
        {:not_atomic,
         "cannot atomically validate the `precision` of a decimal with an expression"}

      constraints[:scale] && constraints[:scale] != :arbitrary ->
        {:not_atomic, "cannot atomically validate the `scale` of a decimal with an expression"}

      true ->
        {:atomic, expr}
    end
  end

  def apply_atomic_constraints(expr, constraints) do
    if Ash.Expr.expr?(expr) do
      expr =
        Enum.reduce(constraints, expr, fn
          {:precision, :arbitrary}, expr ->
            expr

          {:scale, :arbitrary}, expr ->
            expr

          {:max, max}, expr ->
            expr(
              if ^expr > ^max do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("must be less than or equal to %{max}"),
                  vars: %{max: ^max}
                )
              else
                ^expr
              end
            )

          {:min, min}, expr ->
            expr(
              if ^expr < ^min do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("must be greater than or equal to %{min}"),
                  vars: %{min: ^min}
                )
              else
                ^expr
              end
            )

          {:less_than, less_than}, expr ->
            expr(
              if ^expr < ^less_than do
                ^expr
              else
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("must be less than %{less_than}"),
                  vars: %{less_than: ^less_than}
                )
              end
            )

          {:greater_than, greater_than}, expr ->
            expr(
              if ^expr > ^greater_than do
                ^expr
              else
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("must be greater than %{greater_than}"),
                  vars: %{greater_than: ^greater_than}
                )
              end
            )
        end)

      {:ok, expr}
    else
      apply_constraints(expr, constraints)
    end
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    errors =
      Enum.reduce(constraints, [], fn
        {:precision, :arbitrary}, errors ->
          errors

        {:precision, precision}, errors ->
          if count_significant_digits(value) > precision do
            [
              [
                message: error_message("must have no more than %{precision} significant digits"),
                precision: precision
              ]
              | errors
            ]
          else
            errors
          end

        {:scale, :arbitrary}, errors ->
          errors

        {:scale, scale}, errors ->
          if Decimal.scale(value) > scale do
            [
              [
                message: error_message("must have no more than %{scale} decimal places"),
                scale: scale
              ]
              | errors
            ]
          else
            errors
          end

        {:max, max}, errors ->
          if Decimal.compare(value, max) == :gt do
            [[message: error_message("must be less than or equal to %{max}"), max: max] | errors]
          else
            errors
          end

        {:min, min}, errors ->
          if Decimal.compare(value, min) == :lt do
            [
              [message: error_message("must be greater than or equal to %{min}"), min: min]
              | errors
            ]
          else
            errors
          end

        {:less_than, less_than}, errors ->
          if Decimal.compare(value, less_than) == :lt do
            errors
          else
            [
              [message: error_message("must be less than %{less_than}"), less_than: less_than]
              | errors
            ]
          end

        {:greater_than, greater_than}, errors ->
          if Decimal.compare(value, greater_than) == :gt do
            errors
          else
            [
              [
                message: error_message("must be greater than %{greater_than}"),
                greater_than: greater_than
              ]
              | errors
            ]
          end
      end)

    case errors do
      [] -> {:ok, value}
      errors -> {:error, errors}
    end
  end

  @impl true
  def coerce(value, _) do
    cast_input(value, [])
  end

  @impl true
  def cast_input(value, _constraints) when is_binary(value) do
    case Decimal.parse(value) do
      {decimal, ""} ->
        {:ok, decimal}

      _ ->
        :error
    end
  end

  @impl true
  def cast_input(value, _constraints) do
    case Ecto.Type.cast(:decimal, value) do
      {:ok, decimal} ->
        {:ok, decimal}

      error ->
        error
    end
  end

  @impl true

  def cast_stored(value, _) when is_binary(value) do
    case Decimal.parse(value) do
      {decimal, ""} ->
        {:ok, decimal}

      _ ->
        :error
    end
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:decimal, value)
  end

  @impl true
  @spec dump_to_native(any, any) :: :error | {:ok, any}
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:decimal, value)
  end

  @doc false
  def new(%Decimal{} = v), do: v
  def new(v), do: Decimal.new(v)

  @impl true
  def equal?(nil, nil), do: true
  def equal?(nil, _right), do: false
  def equal?(_left, nil), do: false
  def equal?(left, right), do: Decimal.eq?(left, right)

  # Helper function to count significant digits in a decimal
  defp count_significant_digits(%Decimal{coef: coef}) do
    if coef == 0 do
      # Zero has 1 significant digit
      1
    else
      # Convert coefficient to string and count digits
      coef_str = Integer.to_string(coef)
      String.length(coef_str)
    end
  end

  # Generator helpers
  def nudge(nil, _), do: nil
  def nudge(%Decimal{} = decimal, :down), do: Decimal.sub(decimal, nudge_amount(decimal))
  def nudge(%Decimal{} = decimal, :up), do: Decimal.add(decimal, nudge_amount(decimal))

  defp nudge_amount(%Decimal{} = decimal) do
    decimal
    |> Decimal.normalize()
    |> Decimal.scale()
    |> min(0)
    |> Kernel.-(1)
    |> then(&Decimal.new(1, 10, &1))
  end

  defp trunc_precision(decimal, :arbitrary), do: decimal

  defp trunc_precision(%Decimal{sign: sign, coef: coef, exp: exp} = decimal, precision) do
    diff = count_significant_digits(decimal) - precision

    if diff > 0 do
      Decimal.new(sign, trunc_coef(coef, diff), exp + diff)
      |> Decimal.normalize()
    else
      decimal
    end
  end

  defp trunc_scale(decimal, :arbitrary), do: decimal

  defp trunc_scale(%Decimal{sign: sign, coef: coef, exp: exp} = decimal, scale) do
    max_exp = max(exp, -1 * scale)
    diff = max_exp - exp

    if diff > 0 do
      Decimal.new(sign, trunc_coef(coef, diff), max_exp)
      |> Decimal.normalize()
    else
      decimal
    end
  end

  defp trunc_coef(coef, num_digits) do
    coef
    |> Integer.digits()
    |> Enum.slice(0..-(num_digits + 1)//1)
    |> Integer.undigits()
  end

  defp maximum(nil, nil), do: nil
  defp maximum(v1, nil), do: to_number(v1)
  defp maximum(nil, v2), do: to_number(v2)
  defp maximum(v1, v2), do: max(to_number(v1), to_number(v2))

  defp minimum(nil, nil), do: nil
  defp minimum(v1, nil), do: to_number(v1)
  defp minimum(nil, v2), do: to_number(v2)
  defp minimum(v1, v2), do: min(to_number(v1), to_number(v2))

  defp to_number(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp to_number(number), do: number
end

import Ash.Type.Comparable

defcomparable left :: Decimal, right :: Integer do
  Decimal.compare(left, Ash.Type.Decimal.new(right))
end

defcomparable left :: Decimal, right :: Decimal do
  Decimal.compare(left, right)
end

defcomparable left :: Decimal, right :: Float do
  Decimal.compare(Ash.Type.Decimal.new(left), right)
end

defcomparable left :: Decimal, right :: BitString do
  Decimal.compare(left, Ash.Type.Decimal.new(right))
end
