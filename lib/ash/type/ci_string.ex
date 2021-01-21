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
          cond do
            !String.contains?(Regex.opts(regex), "i") ->
              [
                [
                  message:
                    "must provide a case insensitive regex (using the `i` modifier), got: %{regex}",
                  regex: regex
                ]
                | errors
              ]

            String.match?(value, regex) ->
              errors

            true ->
              [[message: "must match the pattern %{regex}", regex: inspect(regex)] | errors]
          end
      end)

    case errors do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  @impl true
  def cast_input(value) do
    case Ecto.Type.cast(:string, value) do
      {:ok, value} -> {:ok, String.downcase(value)}
      :error -> :error
    end
  end

  @impl true
  def cast_stored(value) do
    case Ecto.Type.load(:string, value) do
      {:ok, value} -> {:ok, String.downcase(value)}
      :error -> :error
    end
  end

  @impl true
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
