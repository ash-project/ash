defmodule Ash.Type.Float do
  @constraints [
    max: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a maximum on the value"
    ],
    min: [
      type: {:custom, __MODULE__, :float, []},
      doc: "Enforces a minimum on the value"
    ]
  ]

  require Ash.Expr

  @moduledoc """
  Represents a float (floating point number)

  A builtin type that be referenced via `:float`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
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
    StreamData.float(Keyword.take(constraints, [:min, :max]))
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

  def cast_atomic(expr, constraints) do
    expr =
      case {constraints[:max], constraints[:max]} do
        {nil, nil} ->
          expr

        {max, nil} ->
          Ash.Expr.expr(
            if ^expr > ^max do
              error(
                Ash.Error.Changes.InvalidChanges,
                message: "must be less than or equal to %{max}",
                vars: %{max: max}
              )
            else
              ^expr
            end
          )

        {nil, min} ->
          Ash.Expr.expr(
            if ^expr > ^min do
              error(
                Ash.Error.Changes.InvalidChanges,
                message: "must be greater than or equal to %{min}",
                vars: %{min: min}
              )
            else
              ^expr
            end
          )

        {max, min} ->
          Ash.Expr.expr(
            cond do
              ^expr < ^min ->
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: "must be greater than or equal to %{min}",
                  vars: %{min: min}
                )

              ^expr > ^max ->
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: "must be less than or equal to %{max}",
                  vars: %{max: max}
                )

              true ->
                ^expr
            end
          )
      end

    {:atomic, expr}
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:float, value)
  end
end
