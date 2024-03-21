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
    gt: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a minimum on the value (exclusive)"
    ],
    lt: [
      type: {:custom, __MODULE__, :decimal, []},
      doc: "Enforces a maximum on the value (exclusive)"
    ]
  ]
  @moduledoc """
  Represents a decimal.

  A builtin type that can be referenced via `:decimal`

  ### Constraints

  #{Spark.OptionsHelpers.docs(@constraints)}
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
      (!constraints[:max] || !Decimal.gt?(value, constraints[:max])) &&
        (!constraints[:lt] || Decimal.lt?(value, constraints[:lt])) &&
        (!constraints[:min] || !Decimal.lt?(value, constraints[:min])) &&
        (!constraints[:gt] || Decimal.gt?(value, constraints[:gt]))
    end)
  end

  @impl true
  def storage_type(_), do: :decimal

  @impl true
  def constraints, do: @constraints

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

        {:lt, lt}, errors ->
          if Decimal.compare(value, lt) == :lt do
            errors
          else
            [[message: "must be less than %{lt}", lt: lt] | errors]
          end

        {:gt, gt}, errors ->
          if Decimal.compare(value, gt) == :gt do
            errors
          else
            [[message: "must be more than %{gt}", gt: gt] | errors]
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
end
