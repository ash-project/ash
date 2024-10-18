defmodule Ash.Type.Decimal do
  @constraints [
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

  def cast_atomic(expr, _constraints), do: {:atomic, expr}

  def apply_atomic_constraints(expr, constraints) do
    expr =
      Enum.reduce(constraints, expr, fn
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
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    errors =
      Enum.reduce(constraints, [], fn
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
  def cast_input(value, _) when is_binary(value) do
    case Decimal.parse(value) do
      {decimal, ""} ->
        {:ok, decimal}

      _ ->
        :error
    end
  end

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:decimal, value)
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
  def equal?(left, right) do
    Decimal.eq?(left, right)
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
