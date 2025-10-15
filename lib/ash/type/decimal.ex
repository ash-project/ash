# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
    params =
      constraints
      |> Keyword.take([:min, :max])
      |> Enum.map(fn {key, value} ->
        if Decimal.is_decimal(value) do
          {key, Decimal.to_float(value)}
        else
          {key, value}
        end
      end)

    params
    |> StreamData.float()
    |> StreamData.map(&Decimal.from_float/1)
    #  A second pass filter to account for inaccuracies in the above float -> decimal
    |> StreamData.filter(fn value ->
      !(constraints[:max] && Decimal.gt?(value, constraints[:max])) &&
        (!constraints[:less_than] || Decimal.lt?(value, constraints[:less_than])) &&
        !(constraints[:min] && Decimal.lt?(value, constraints[:min])) &&
        (!constraints[:greater_than] || Decimal.gt?(value, constraints[:greater_than]))
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
                  message: "must be less than or equal to %{max}",
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
                  message: "must be greater than or equal to %{min}",
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
                  message: "must be less than %{less_than}",
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
                  message: "must be greater than %{greater_than}",
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
                message: "must have no more than %{precision} significant digits",
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
                message: "must have no more than %{scale} decimal places",
                scale: scale
              ]
              | errors
            ]
          else
            errors
          end

        {:max, max}, errors ->
          if Decimal.compare(value, max) == :gt do
            [[message: "must be less than or equal to %{max}", max: max] | errors]
          else
            errors
          end

        {:min, min}, errors ->
          if Decimal.compare(value, min) == :lt do
            [[message: "must be more than or equal to %{min}", min: min] | errors]
          else
            errors
          end

        {:less_than, less_than}, errors ->
          if Decimal.compare(value, less_than) == :lt do
            errors
          else
            [[message: "must be less than %{less_than}", less_than: less_than] | errors]
          end

        {:greater_than, greater_than}, errors ->
          if Decimal.compare(value, greater_than) == :gt do
            errors
          else
            [[message: "must be more than %{greater_than}", greater_than: greater_than] | errors]
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
