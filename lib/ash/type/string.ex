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
      end)

    case errors do
      [] -> :ok
      errors -> {:error, errors}
    end
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
