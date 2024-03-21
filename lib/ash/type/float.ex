defmodule Ash.Type.Float do
  @constraints [
    max: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a maximum on the value"
    ],
    min: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a minimum on the value"
    ],
    greater_than: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a minimum on the value (exclusive)"
    ],
    less_than: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a maximum on the value (exclusive)"
    ]
  ]
  @moduledoc """
  Represents a float (floating point number)

  A builtin type that be referenced via `:float`

  ### Constraints

  #{Spark.OptionsHelpers.docs(@constraints)}
  """

  use Ash.Type

  @impl true
  def storage_type(_), do: :float

  @impl true
  def constraints, do: @constraints

  @doc false
  def float(value) do
    case cast_input(value, []) do
      {:ok, float} ->
        {:ok, float}

      :error ->
        {:error, "cannot be casted to float"}
    end
  end

  @impl true
  def generator(constraints) do
    constraints
    |> Keyword.take([:min, :max])
    |> StreamData.float()
    |> StreamData.filter(fn value ->
      (!constraints[:less_than] || value < constraints[:less_than]) &&
        (!constraints[:greater_than] || value > constraints[:greater_than])
    end)
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    errors =
      Enum.reduce(constraints, [], fn
        {:max, max}, errors ->
          if value > max do
            [[message: "must be less than or equal to %{max}", max: max] | errors]
          else
            errors
          end

        {:min, min}, errors ->
          if value < min do
            [[message: "must be more than or equal to %{min}", min: min] | errors]
          else
            errors
          end

        {:less_than, less_than}, errors ->
          if value < less_than do
            errors
          else
            [[message: "must be less than %{less_than}", less_than: less_than] | errors]
          end

        {:greater_than, greater_than}, errors ->
          if value > greater_than do
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
  def cast_input(value, _) do
    Ecto.Type.cast(:float, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:float, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:float, value)
  end
end
