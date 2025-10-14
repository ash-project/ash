# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
            description: [
              type: :string
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
              description: "The amount of the transaction",
              constraints: [
                max: 10
              ]
            ],
            currency: [
              type: :string,
              allow_nil?: false,
              description: "The currency code of the transaction",
              constraints: [
                max_length: 3
              ]
            ]
          ]

      allow_nil? is true by default
      """
    ]
  ]

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
  def matches_type?(v, _constraints) do
    Keyword.keyword?(v)
  end

  @impl true
  def init(constraints) do
    if is_list(constraints[:fields]) do
      constraints[:fields]
      |> List.wrap()
      |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, fields} ->
        type = Ash.Type.get_type(config[:type])
        constraints = config[:constraints] || []

        if Keyword.get(config, :init?, true) do
          case Ash.Type.init(type, constraints) do
            {:ok, constraints} ->
              {:cont,
               {:ok,
                [{name, Keyword.merge(config, constraints: constraints, type: type)} | fields]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        else
          {:cont, {:ok, [{name, config} | fields]}}
        end
      end)
      |> case do
        {:ok, fields} ->
          {:ok, Keyword.put(constraints, :fields, Enum.reverse(fields))}

        {:error, error} ->
          {:error, error}
      end
    else
      if is_nil(constraints[:fields]) do
        {:ok, constraints}
      else
        {:error, "fields must be a list, got `#{constraints[:fields]}`"}
      end
    end
  end

  @impl true
  def cast_input("", _), do: {:ok, nil}

  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_binary(value) do
    case Ash.Helpers.json_module().decode(value) do
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
      {:ok, Map.new(value)}
    else
      :error
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

  @impl true
  def generator(constraints) do
    Ash.Type.Map.generator(constraints)
    |> StreamData.map(fn value ->
      Map.to_list(value)
    end)
  end

  defp check_fields(value, fields) do
    {errors, result} =
      Enum.reduce(fields, {[], []}, fn {field, field_constraints}, {errors_acc, result_acc} ->
        case fetch_field(value, field) do
          {:ok, field_value} ->
            case check_field(result_acc, field, field_value, field_constraints) do
              {:ok, updated_result} ->
                {errors_acc, updated_result}

              {:error, field_errors} ->
                {errors_acc ++ field_errors, result_acc}
            end

          :error ->
            if field_constraints[:allow_nil?] == false do
              field_error = [message: "field must be present", field: field]
              {errors_acc ++ [field_error], result_acc}
            else
              {errors_acc, result_acc}
            end
        end
      end)

    case errors do
      [] -> {:ok, result}
      _ -> {:error, errors}
    end
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
            {:error,
             Ash.Type.CompositeTypeHelpers.convert_constraint_errors_to_keyword_lists(
               errors,
               field
             )}
        end

      {:error, error} ->
        {:error, [error]}

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
