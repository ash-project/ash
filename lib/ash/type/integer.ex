defmodule Ash.Type.Integer do
  @moduledoc "Stores an integer in the database"
  use Ash.Type

  @impl true
  def storage_type, do: :integer

  @impl true
  def constraints do
    [
      max: [
        type: :non_neg_integer,
        doc: "Enforces a maximum on the value"
      ],
      min: [
        type: :non_neg_integer,
        doc: "Enforces a minimum on the value"
      ]
    ]
  end

  def apply_constraints(value, constraints) do
    errors =
      Enum.reduce(constraints, [], fn
        {:max, max}, errors ->
          if value > max do
            ["must be less than `#{max}`" | errors]
          else
            errors
          end

        {:min, min}, errors ->
          if value < min do
            ["must be more than `#{min}`" | errors]
          else
            errors
          end
      end)

    case errors do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  @impl true
  def cast_input(value) do
    Ecto.Type.cast(:integer, value)
  end

  @impl true
  def cast_stored(value) do
    Ecto.Type.load(:integer, value)
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:integer, value)
  end
end
