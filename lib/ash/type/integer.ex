defmodule Ash.Type.Integer do
  @constraints [
    max: [
      type: {:custom, __MODULE__, :integer, []},
      doc: "Enforces a maximum on the value"
    ],
    min: [
      type: {:custom, __MODULE__, :integer, []},
      doc: "Enforces a minimum on the value"
    ]
  ]
  @moduledoc """
  Represents a simple integer

  A builtin type that can be referenced via `:integer`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  require Ash.Expr

  @impl true

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
  def storage_type(_), do: :integer

  @impl true
  def generator(constraints) do
    min = constraints[:min] || -2_147_483_648
    max = constraints[:max] || 2_147_483_647

    StreamData.integer(min..max)
  end

  @impl true
  def constraints, do: @constraints

  @doc false
  def integer(value) when is_integer(value), do: {:ok, value}
  def integer(_), do: {:error, "must be an integer"}

  def apply_constraints(nil, _), do: :ok

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
    Ecto.Type.cast(:integer, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(string, constraints) when is_binary(string) do
    case Integer.parse(string) do
      {integer, ""} ->
        cast_stored(integer, constraints)

      _ ->
        :error
    end
  end

  def cast_stored(value, _) do
    Ecto.Type.load(:integer, value)
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:integer, value)
  end
end
