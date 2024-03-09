defmodule Ash.Type.Keyword do
  @constraints [
    fields: [
      required: true,
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
      The types of the fields in the keyword, and their constraints.

      If constraints are specified, only those fields will be in the casted keyword.

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
  Represents a keyword list, stored as a `:map` in the database.

  A builtin type that can be referenced via `:keyword_list`

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

  def cast_input(value, constraints) when is_map(value) do
    value
    |> try_map_to_keyword(constraints)
    |> cast_input(constraints)
  end

  def cast_input(value, constraints) do
    if Keyword.keyword?(value) do
      {:ok, Keyword.take(value, Keyword.keys(constraints[:fields] || []))}
    else
      :error
    end
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_map(value),
    do: {:ok, try_map_to_keyword(value, constraints)}

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_map(value), do: {:ok, value}

  def dump_to_native(value, _) do
    if Keyword.keyword?(value) do
      {:ok, Keyword.new(value)}
    else
      :error
    end
  end

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
        case check_fields(value, fields) do
          {:ok, fields} -> {:ok, Enum.reverse(fields)}
          {:error, error} -> {:error, error}
        end

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  defp check_fields(value, fields) do
    Enum.reduce(fields, {:ok, []}, fn
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
           field_constraints[:type]
         ) do
      {:ok, field_value} ->
        case Ash.Type.apply_constraints(
               field_constraints[:type],
               field_value,
               field_constraints[:constraints]
             ) do
          {:ok, nil} ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "value must not be nil", field: field]]}
            else
              {:ok, [{field, nil} | result]}
            end

          {:ok, field_value} ->
            {:ok, [{field, field_value} | result]}

          {:error, errors} ->
            {:error, Enum.map(errors, fn error -> Keyword.put(error, :field, field) end)}
        end

      :error ->
        {:error, [[message: "invalid value", field: field]]}
    end
  end

  defp fetch_field(keyword, atom) when is_atom(atom) do
    Keyword.fetch(keyword, atom)
  end

  defp fetch_field(keyword, string) when is_binary(string) do
    fetch_field(keyword, String.to_existing_atom(string))
  end

  defp fetch_field(_, _), do: :error

  defp try_map_to_keyword(map, constraints) do
    constraints[:fields]
    |> Kernel.||([])
    |> Enum.flat_map(fn {key, _} ->
      with :error <- Map.fetch(map, key),
           :error <- Map.fetch(map, to_string(key)) do
        []
      else
        {:ok, value} ->
          [{key, value}]
      end
    end)
  end
end
