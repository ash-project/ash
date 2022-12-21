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

  #{Spark.OptionsHelpers.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def constraints, do: @constraints

  @impl true
  def generator(constraints) do
    base_generator =
      StreamData.string(
        :printable,
        Keyword.take(constraints, [:max_length, :min_length])
      )

    if constraints[:trim?] && constraints[:min_length] do
      StreamData.filter(base_generator, fn value ->
        value |> String.trim() |> String.length() |> Kernel.>(constraints[:min_length])
      end)
    else
      base_generator
    end
  end

  def apply_constraints(nil, _), do: :ok

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
