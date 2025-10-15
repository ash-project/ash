# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Tuple do
  @constraints [
    fields: [
      type: :keyword_list,
      required: true,
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
            description: [
              type: :string
            ],
            constraints: [
              type: :keyword_list,
              default: []
            ]
          ]
        ]
      ],
      doc: """
      The types of the fields in the tuple, and their constraints.

      This type is stored as a map under the hood, keyed by the field name,
      and is represented in memory as a tuple.

      Example constraints:

          constraints: fields:  [
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

      `allow_nil?` is `true` by default.
      """
    ]
  ]

  @moduledoc """
  Represents a tuple stored in the data layer as a map.

  A builtin type that can be referenced via `:tuple`

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def init(constraints) do
    constraints[:fields]
    |> List.wrap()
    |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, fields} ->
      type = Ash.Type.get_type(config[:type])
      constraints = config[:constraints] || []

      case Ash.Type.init(type, constraints) do
        {:ok, constraints} ->
          {:cont,
           {:ok, [{name, Keyword.merge(config, constraints: constraints, type: type)} | fields]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, fields} ->
        {:ok, Keyword.put(constraints, :fields, Enum.reverse(fields))}

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def matches_type?(v, _constraints) do
    is_tuple(v)
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_tuple(value) do
    field_length = length(constraints[:fields])
    value_length = tuple_size(value)

    if field_length == value_length do
      {:ok, value}
    else
      {:error,
       message: "Expected %{expected_length} elements, got %{value_length}",
       expected_length: field_length,
       value_length: value_length}
    end
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_map(value) do
    Enum.reduce_while(constraints[:fields], {:ok, []}, fn {key, config}, {:ok, acc} ->
      case fetch_field(value, key) do
        {:ok, value} ->
          case Ash.Type.cast_stored(config[:type], value, config[:constraints] || []) do
            {:ok, value} ->
              {:cont, {:ok, [value | acc]}}

            other ->
              {:halt, other}
          end

        :error ->
          {:cont, {:ok, acc}}
      end
    end)
    |> case do
      {:ok, value} -> {:ok, value |> Enum.reverse() |> List.to_tuple()}
      {:error, error} -> {:error, error}
    end
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, constraints) when is_tuple(value) do
    list = Tuple.to_list(value)

    if length(list) == length(constraints[:fields]) do
      list
      |> Enum.zip(constraints[:fields])
      |> Map.new(fn {tuple_val, {key, _config}} ->
        {key, tuple_val}
      end)
      |> then(&{:ok, &1})
    else
      :error
    end
  end

  def dump_to_native(_, _), do: :error

  @impl true
  def apply_constraints(value, constraints) do
    constraints[:fields]
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn
      {{field, field_constraints}, index}, {:ok, acc} ->
        field_value = elem(value, index)

        case check_field(acc, field, field_value, field_constraints) do
          {:ok, acc} -> {:cont, {:ok, acc}}
          other -> {:halt, other}
        end
    end)
    |> case do
      {:ok, acc} -> {:ok, acc |> Enum.reverse() |> List.to_tuple()}
      other -> other
    end
  end

  @impl true
  def generator(constraints) do
    optional =
      constraints[:fields]
      |> Enum.filter(fn {_, value} ->
        value[:allow_nil?]
      end)
      |> Keyword.keys()

    constraints[:fields]
    |> Map.new(fn {key, config} ->
      type = config[:type]
      constraints = config[:constraints] || []
      {key, Ash.Type.generator(type, constraints)}
    end)
    |> Ash.Generator.mixed_map(optional)
    |> StreamData.map(fn map ->
      Enum.map(constraints[:fields], fn {field, _} ->
        Map.get(map, field)
      end)
      |> List.to_tuple()
    end)
  end

  defp check_field(acc, field, field_value, field_constraints) do
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
              {:ok, [nil | acc]}
            end

          {:ok, field_value} ->
            {:ok, [field_value | acc]}

          {:error, errors} ->
            errors =
              if Keyword.keyword?(errors) do
                [errors]
              else
                List.wrap(errors)
              end

            {:error, Enum.map(errors, fn error -> Keyword.put(error, :field, field) end)}
        end

      {:error, error} ->
        {:error, [error]}

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
