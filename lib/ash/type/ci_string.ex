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
    ]
  ]

  @moduledoc """
  Stores a case insensitive string in the database

  See `Ash.CiString` for more information.

  A builtin type that can be referenced via `:ci_string`

  ### Constraints

  #{Ash.OptionsHelpers.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def constraints, do: @constraints

  def apply_constraints(%Ash.CiString{} = value, constraints) do
    value
    |> Ash.CiString.value()
    |> apply_constraints(constraints)
    |> case do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, value} ->
        {:ok, %Ash.CiString{string: value, lowered?: true}}

      other ->
        other
    end
  end

  def apply_constraints(value, constraints) do
    {value, errors} =
      return_value(constraints[:allow_empty?], constraints[:trim?], value, constraints)

    case errors do
      [] -> {:ok, value}
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
  def cast_input(%Ash.CiString{} = value), do: {:ok, value}

  def cast_input(value) do
    case Ecto.Type.cast(:string, value) do
      {:ok, value} -> {:ok, Ash.CiString.new(value)}
      :error -> :error
    end
  end

  @impl true
  def cast_stored(value) do
    case Ecto.Type.load(:string, value) do
      {:ok, value} -> {:ok, Ash.CiString.new(value)}
      :error -> :error
    end
  end

  @impl true
  def dump_to_native(%Ash.CiString{} = ci_string) do
    case Ecto.Type.dump(:string, Ash.CiString.value(ci_string)) do
      {:ok, value} -> {:ok, value}
      :error -> :error
    end
  end

  def dump_to_native(value) do
    case Ecto.Type.dump(:string, value) do
      {:ok, value} -> {:ok, String.downcase(value)}
      :error -> :error
    end
  end

  def match(%Regex{} = regex), do: {:ok, regex}

  def match(_) do
    {:error, "Must provide a regex to match, e.g ~r/foobar/"}
  end
end
