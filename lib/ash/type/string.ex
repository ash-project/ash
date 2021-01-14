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
      doc: "Sets the value to `nil` if it's empty.",
      default: false
    ]
  ]

  @moduledoc """
  Stores a string in the database.

  A built-in type that can be referenced via `:string`.

  Empty values are, by default, being set to `nil`.
  You can use the `allow_empty?` constraint to change this behavior.

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
    trim_value? = constraints[:trim?]
    allow_empty? = constraints[:allow_empty?]

    trimmed_value = String.trim(value)

    value =
      cond do
        allow_empty? == false && trimmed_value == "" ->
          nil

        trim_value? ->
          trimmed_value

        true ->
          value
      end

    skip_validation? = value == nil || (allow_empty? && trimmed_value == "")

    errors =
      unless skip_validation? do
        validate(value, constraints)
      else
        []
      end

    case errors do
      [] -> {:ok, value}
      errors -> {:error, errors}
    end
  end

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
          [{"must match the pattern %{regex}", regex: inspect(regex)} | errors]
        end

      _, errors ->
        errors
    end)
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
