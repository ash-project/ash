defmodule Ash.Type.CiString do
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
      doc: "Sets the value to `nil` if it's empty.",
      default: false
    ],
    casing: [
      type: {:one_of, [:upper, :lower, nil]},
      default: nil,
      doc: """
      Lowercases or uppercases the value, fully discarding case information.

      For example, if you don't set this, a value of `FrEd` could be saved to the data layer.
      `FrEd` and `fReD` would still compare as equal, but the original casing information  is retained.
      In many cases, this is what you want. In some cases, however, you want to remove all case information.
      For example, in an email, you may want to support a user inputting an upper case letter, but discard it
      when saved.
      """
    ]
  ]

  @moduledoc """
  Stores a case insensitive string in the database

  See `Ash.CiString` for more information.

  A builtin type that can be referenced via `:ci_string`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :ci_string

  @impl true
  def constraints, do: @constraints

  @impl true
  def generator(constraints) do
    StreamData.string(
      :printable,
      Keyword.take(constraints, [:max_length, :min_length])
    )
    |> then(fn generator ->
      cond do
        constraints[:trim?] && constraints[:min_length] ->
          StreamData.filter(generator, fn value ->
            value |> String.trim() |> String.length() |> Kernel.>=(constraints[:min_length])
          end)

        constraints[:min_length] ->
          StreamData.filter(generator, fn value ->
            value |> String.length() |> Kernel.>=(constraints[:min_length])
          end)

        true ->
          generator
      end
    end)
    |> StreamData.map(fn value ->
      value =
        case constraints[:casing] do
          :lower ->
            String.downcase(value)

          :upper ->
            String.upcase(value)

          nil ->
            value
        end

      Ash.CiString.new(value, constraints[:casing])
    end)
  end

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def matches_type?(%Ash.CiString{}, _), do: true
  def matches_type?(_, _), do: false

  def apply_constraints(%Ash.CiString{} = value, constraints) do
    value
    |> Ash.CiString.value()
    |> apply_constraints(constraints)
    |> case do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, value} ->
        {:ok, Ash.CiString.new(value, constraints[:casing])}

      other ->
        other
    end
  end

  def apply_constraints(nil, _), do: {:ok, nil}

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
  def cast_input(%Ash.CiString{} = value, _), do: {:ok, value}

  def cast_input(value, _) do
    case Ecto.Type.cast(:string, value) do
      {:ok, value} -> {:ok, Ash.CiString.new(value)}
      :error -> :error
    end
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(%Ash.CiString{} = value, _), do: {:ok, value}

  def cast_stored(value, _) do
    case Ecto.Type.load(:string, value) do
      {:ok, value} -> {:ok, Ash.CiString.new(value)}
      :error -> :error
    end
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(%Ash.CiString{} = ci_string, _) do
    case Ecto.Type.dump(:string, Ash.CiString.value(ci_string)) do
      {:ok, value} -> {:ok, value}
      :error -> :error
    end
  end

  def dump_to_native(value, _) do
    case Ecto.Type.dump(:string, value) do
      {:ok, value} -> {:ok, value}
      :error -> :error
    end
  end

  @impl true
  def equal?(left, right) do
    Ash.CiString.compare(left, right) == :eq
  end

  def match(%Regex{} = regex), do: {:ok, regex}

  def match(_) do
    {:error, "Must provide a regex to match, e.g ~r/foobar/"}
  end
end
