defmodule Ash.Type.String do
  @empty_characters ["\u00A0", "\u2000", "\u2001", "\u2002", "\u2003"] ++
                      ["\u2004", "\u2005", "\u2006", "\u2007", "\u2008"] ++
                      ["\u2009", "\u200A", "\u2028", "\u205F", "\u3000"]

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
    empty_values: [
      type: :any,
      doc:
        "A list of characters which get replaced by a value defined in empty_value_replacement. " <>
          "To avoid replacing any values, set to an empty list",
      default: @empty_characters
    ],
    empty_value_replacement: [
      type: :binary,
      doc:
        "A value that is used to replace any values defined in empty_values. Defaults to a single space",
      default: "\u0020"
    ],
    trim_value?: [
      type: :boolean,
      doc: "Trims the value and sets it to nil if it's empty",
      default: true
    ]
  ]

  @moduledoc """
  Stores a string in the database

  A builtin type that can be referenced via `:string`

  ### Constraints

  #{Ash.OptionsHelpers.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def constraints, do: @constraints

  def apply_constraints(nil, _), do: :ok

  def apply_constraints(value, constraints) do
    value =
      replace_characters(value, constraints[:empty_values], constraints[:empty_value_replacement])

    trim_value? = constraints[:trim_value?]
    trimmed_value = String.trim(value)

    value =
      if trim_value? do
        trimmed_value
      else
        value
      end

    errors =
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
            [{"must match the pattern %{regex}", regex: inspect(regex)} | errors]
          end

        _, errors ->
          errors
      end)

    value =
      if trim_value? && value == "" do
        nil
      else
        value
      end

    case errors do
      [] -> {:ok, value}
      errors -> {:error, errors}
    end
  end

  defp replace_characters(value, characters, replacement) do
    Enum.reduce(characters, value, &String.replace(&2, &1, replacement))
  end

  @impl true
  def cast_input(value) do
    Ecto.Type.cast(:string, value)
  end

  @impl true
  def cast_stored(value) do
    Ecto.Type.load(:string, value)
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:string, value)
  end

  def match(%Regex{} = regex), do: {:ok, regex}

  def match(_) do
    {:error, "Must provide a regex to match, e.g ~r/foobar/"}
  end
end
