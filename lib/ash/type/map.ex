defmodule Ash.Type.Map do
  @constraints [
    fields: [
      type: :keyword_list,
      keys: [
        *: [
          type: :keyword_list,
          keys: [
            type: [
              type: Ash.OptionsHelpers.ash_type(),
              required: true
            ],
            allow_nil?: [
              type: :boolean,
              default: true
            ],
            constraints: [
              type: :keyword_list,
              default: []
            ]
          ]
        ]
      ],
      doc: """
      The types of the fields in the map, and their constraints.

      If constraints are specified, only those fields will be in the casted map.

      For example:

          fields:  [
            amount: [
              type: :integer,
              constraints: [
                max: 10
              ]
            ],
            currency: [
              type: :string,
              allow_nil?: false,
              constraints: [
                max_length: 3
              ]
            ]
          ]

      allow_nil? is true by default
      """
    ]
  ]

  def field_types(value) do
    {:ok, value}
  end

  @moduledoc """
  Represents a map stored in the database.

  In postgres, for example, this represents binary encoded json

  A builtin type that can be referenced via `:map`

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def cast_input("", _), do: {:ok, nil}

  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_binary(value) do
    case Jason.decode(value) do
      {:ok, value} ->
        cast_input(value, constraints)

      _ ->
        :error
    end
  end

  def cast_input(value, _) when is_map(value), do: {:ok, value}
  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) when is_map(value), do: {:ok, value}

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_map(value), do: {:ok, value}
  def dump_to_native(_, _), do: :error

  @impl true
  def cast_atomic(new_value, constraints) do
    if constraints[:keys] do
      {:not_atomic, "Keywords do not support atomic updates when using the `keys` constraint"}
    else
      {:atomic, new_value}
    end
  end

  @impl true
  def apply_constraints(value, constraints) do
    Enum.reduce(constraints, {:ok, value}, fn
      {:fields, fields}, {:ok, value} ->
        check_fields(value, fields)

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  defp check_fields(value, fields) do
    Enum.reduce(fields, {:ok, %{}}, fn
      {field, field_constraints}, {:ok, checked_value} ->
        case fetch_field(value, field) do
          {:ok, field_value} ->
            check_field(checked_value, field, field_value, field_constraints)

          :error ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "field must be present", field: field]]}
            else
              {:ok, checked_value}
            end
        end

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  defp check_field(result, field, field_value, field_constraints) do
    case Ash.Type.cast_input(
           field_constraints[:type],
           field_value,
           field_constraints[:constraints] || []
         ) do
      {:ok, field_value} ->
        case Ash.Type.apply_constraints(
               field_constraints[:type],
               field_value,
               field_constraints[:constraints] || []
             ) do
          {:ok, nil} ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "value must not be nil", field: field]]}
            else
              {:ok, Map.put(result, field, nil)}
            end

          {:ok, field_value} ->
            {:ok, Map.put(result, field, field_value)}

          {:error, errors} ->
            {:error, Enum.map(errors, fn error -> Keyword.put(error, :field, field) end)}
        end

      :error ->
        {:error, [[message: "invalid value", field: field]]}
    end
  end

  defp fetch_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> {:ok, value}
      :error -> fetch_field(map, to_string(atom))
    end
  end

  defp fetch_field(map, key), do: Map.fetch(map, key)
end
