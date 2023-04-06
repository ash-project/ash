defmodule Ash.Type.Map do
  # fields: [
  #   type: :keyword_list,
  #   keys: [
  #     *: [
  #       type: :keyword_list,
  #       keys: [
  #         type: [
  #           type: Ash.OptionsHelpers.ash_type(),
  #           required: true
  #         ],
  #         constraints: [
  #           type: :keyword,
  #           default: []
  #         ]
  #       ]
  #     ]
  #   ],

  @constraints [
    fields: [
      type: {:custom, __MODULE__, :field_types, []},
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

  #{Spark.OptionsHelpers.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type, do: :map

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

  def cast_input(value, constraints) when is_map(value), do: check_constraints(value, constraints)
  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) when is_map(value), do: {:ok, value}

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_map(value), do: {:ok, value}
  def dump_to_native(_, _), do: :error

  defp check_constraints(value, constraints) do
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
          nil ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "must not be nil", field: field]]}
            else
              {:ok, Map.put(checked_value, field, nil)}
            end

          field_value ->
            case Ash.Type.cast_input(
                   field_constraints[:type],
                   field_value,
                   field_constraints[:constraints]
                 ) do
              {:ok, field_value} ->
                {:ok, Map.put(checked_value, field, field_value)}

              {:error, errors} ->
                {:error, errors}
            end
        end

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  defp fetch_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> value
      :error -> fetch_field(map, to_string(atom))
    end
  end

  defp fetch_field(map, key), do: Map.get(map, key)
end
