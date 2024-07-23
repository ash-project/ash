defmodule Ash.Type.String do
  @constraints [
    max_length: [
      type: :non_neg_integer,
      doc: "Enforces a maximum length on the value"
    ],
    min_length: [
      type: :non_neg_integer,
      doc: "Enforces a minimum length on the value"
    ],
    match: [
      type: {:custom, __MODULE__, :match, []},
      doc: "Enforces that the string matches a passed in regex"
    ],
    trim?: [
      type: :boolean,
      doc: "Trims the value.",
      default: true
    ],
    allow_empty?: [
      type: :boolean,
      doc: "If false, the value is set to `nil` if it's empty.",
      default: false
    ]
  ]

  @moduledoc """
  Stores a string in the database.

  A built-in type that can be referenced via `:string`.

  By default, values are trimmed and empty values are set to `nil`.
  You can use the `allow_empty?` and `trim?` constraints to change these behaviors.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  require Ash.Expr

  @impl true
  def storage_type(_), do: :string

  @impl true
  def matches_type?(v, _) do
    is_binary(v)
  end

  @impl true
  def cast_atomic(expr, constraints) when is_binary(expr) do
    with {:ok, value} <- cast_input(expr, constraints) do
      {:atomic, value}
    end
  end

  @impl true
  def cast_atomic(expr, constraints) do
    # We can't support `match` currently, as we don't have a multi-target regex
    if constraints[:match] do
      {:not_atomic, "cannot use the `match` string constraint atomically"}
    else
      expr =
        if constraints[:trim?] do
          Ash.Expr.expr(string_trim(^expr))
        else
          expr
        end

      expr =
        if constraints[:allow_empty?] do
          expr
        else
          Ash.Expr.expr(
            if ^expr == "" do
              nil
            else
              ^expr
            end
          )
        end

      {:atomic, expr}
    end
  end

  @impl true
  def apply_atomic_constraints(expr, constraints) do
    if Ash.Expr.expr?(expr) do
      validated =
        case {constraints[:max_length], constraints[:min_length]} do
          {nil, nil} ->
            expr

          {max, nil} ->
            Ash.Expr.expr(
              if string_length(^expr) > ^max do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: "length must be less than or equal to %{max}",
                  vars: %{max: max}
                )
              else
                ^expr
              end
            )

          {nil, min} ->
            Ash.Expr.expr(
              if string_length(^expr) < ^min do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: "length must be greater than or equal to %{min}",
                  vars: %{min: min}
                )
              else
                ^expr
              end
            )

          {max, min} ->
            Ash.Expr.expr(
              cond do
                string_length(^expr) < ^min ->
                  error(
                    Ash.Error.Changes.InvalidChanges,
                    message: "length must be greater than or equal to %{min}",
                    vars: %{min: min}
                  )

                string_length(^expr) > ^max ->
                  error(
                    Ash.Error.Changes.InvalidChanges,
                    message: "length must be less than or equal to %{max}",
                    vars: %{max: max}
                  )

                true ->
                  ^expr
              end
            )
        end

      {:ok, validated}
    else
      apply_constraints(expr, constraints)
    end
  end

  @impl true
  def constraints, do: @constraints

  @impl true
  def generator(constraints) do
    base_generator =
      StreamData.string(
        :printable,
        Keyword.take(constraints, [:max_length, :min_length])
      )

    cond do
      constraints[:trim?] && constraints[:min_length] ->
        StreamData.filter(base_generator, fn value ->
          value |> String.trim() |> String.length() |> Kernel.>=(constraints[:min_length])
        end)

      constraints[:min_length] ->
        StreamData.filter(base_generator, fn value ->
          value |> String.length() |> Kernel.>=(constraints[:min_length])
        end)

      true ->
        base_generator
    end
  end

  def apply_constraints(nil, _), do: :ok

  def apply_constraints(value, constraints) do
    {value, errors} =
      return_value(
        Keyword.get(constraints, :allow_empty?, false),
        Keyword.get(constraints, :trim?, true),
        value,
        constraints
      )

    case errors do
      [] -> {:ok, value}
      [error] -> {:error, error}
      errors -> {:error, errors}
    end
  end

  defp return_value(false, true, value, constraints) do
    trimmed = String.trim(value)

    if trimmed == "" do
      {nil, []}
    else
      {trimmed, validate(trimmed, constraints)}
    end
  end

  defp return_value(false, false, value, constraints) do
    if String.trim(value) == "" do
      {nil, []}
    else
      {value, validate(value, constraints)}
    end
  end

  defp return_value(true, true, value, constraints) do
    trimmed = String.trim(value)
    {trimmed, validate(trimmed, constraints)}
  end

  defp return_value(true, false, value, constraints),
    do: {value, validate(value, constraints)}

  defp validate(value, constraints) do
    Enum.reduce(constraints, [], fn
      {:max_length, max_length}, errors ->
        if String.length(value) > max_length do
          [[message: "length must be less than or equal to %{max}", max: max_length] | errors]
        else
          errors
        end

      {:min_length, min_length}, errors ->
        if String.length(value) < min_length do
          [
            [message: "length must be greater than or equal to %{min}", min: min_length]
            | errors
          ]
        else
          errors
        end

      {:match, regex}, errors ->
        if String.match?(value, regex) do
          errors
        else
          [[message: "must match the pattern %{regex}", regex: inspect(regex)] | errors]
        end

      _, errors ->
        errors
    end)
  end

  @impl true
  def cast_input(%Ash.CiString{} = ci_string, constraints) do
    ci_string
    |> Ash.CiString.value()
    |> cast_input(constraints)
  end

  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_atom(value) do
    cast_input(to_string(value), constraints)
  end

  def cast_input(value, _) do
    Ecto.Type.cast(:string, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:string, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:string, value)
  end

  def match(%Regex{} = regex), do: {:ok, regex}

  def match(_) do
    {:error, "Must provide a regex to match, e.g ~r/foobar/"}
  end
end
