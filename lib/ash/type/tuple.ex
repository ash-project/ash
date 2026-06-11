# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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
            ],
            init?: [
              type: :boolean,
              default: true,
              doc: """
              If false, the field's type constraints are not initialised at compile time. \
              Allows for recursive tuple fields.
              """
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
  import Ash.Gettext

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

  @impl true
  def init(constraints) do
    constraints[:fields]
    |> List.wrap()
    |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, fields} ->
      type = Ash.Type.get_type(config[:type])
      constraints = config[:constraints] || []

      if Keyword.get(config, :init?, true) do
        case Ash.Type.init(type, constraints) do
          {:ok, constraints} ->
            {:cont,
             {:ok, [{name, Keyword.merge(config, constraints: constraints, type: type)} | fields]}}

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
  end

  @impl true
  def matches_type?(v, _constraints) do
    is_tuple(v)
  end

  @impl true
  def can_load?(constraints) do
    case constraints[:fields] do
      fields when is_list(fields) ->
        Enum.any?(fields, fn {_name, config} ->
          inner_type = config[:type]
          inner_constraints = config[:constraints] || []
          inner_type && Ash.Type.can_load?(inner_type, inner_constraints)
        end)

      _ ->
        false
    end
  end

  @impl true
  def load(values, load, constraints, context) when is_list(values) do
    fields = constraints[:fields] || []
    indexed_fields = Enum.with_index(fields)

    indexed_fields
    |> effective_load(load)
    |> Enum.reduce_while({:ok, values}, fn {index, key, sub_load}, {:ok, values} ->
      {^key, config} = Enum.at(fields, index)
      inner_type = config[:type]
      inner_constraints = config[:constraints] || []

      load_pos_across(values, index, inner_type, inner_constraints, sub_load, context)
    end)
  end

  defp load_pos_across(
         values,
         index,
         {:array, _} = inner_type,
         inner_constraints,
         sub_load,
         context
       ) do
    values
    |> Enum.reduce_while({:ok, []}, fn
      tuple, {:ok, acc} when is_tuple(tuple) and tuple_size(tuple) > index ->
        case elem(tuple, index) do
          nil ->
            {:cont, {:ok, [tuple | acc]}}

          list when is_list(list) ->
            case Ash.Type.load(inner_type, list, sub_load, inner_constraints, context) do
              {:ok, loaded} -> {:cont, {:ok, [put_elem(tuple, index, loaded) | acc]}}
              {:error, error} -> {:halt, {:error, error}}
            end

          other ->
            {:cont, {:ok, [put_elem(tuple, index, other) | acc]}}
        end

      other, {:ok, acc} ->
        {:cont, {:ok, [other | acc]}}
    end)
    |> case do
      {:ok, reversed} -> {:cont, {:ok, Enum.reverse(reversed)}}
      {:error, error} -> {:halt, {:error, error}}
    end
  end

  defp load_pos_across(values, index, inner_type, inner_constraints, sub_load, context) do
    slice =
      Enum.map(values, fn
        tuple when is_tuple(tuple) and tuple_size(tuple) > index -> elem(tuple, index)
        _ -> nil
      end)

    case Ash.Type.load(inner_type, slice, sub_load, inner_constraints, context) do
      {:ok, loaded_slice} ->
        updated =
          values
          |> Enum.zip(loaded_slice)
          |> Enum.map(fn
            {tuple, v} when is_tuple(tuple) and tuple_size(tuple) > index ->
              put_elem(tuple, index, v)

            {other, _v} ->
              other
          end)

        {:cont, {:ok, updated}}

      {:error, error} ->
        {:halt, {:error, error}}
    end
  end

  defp effective_load(indexed_fields, load) do
    explicit_keys =
      load
      |> List.wrap()
      |> Enum.flat_map(fn
        {k, sub} when is_atom(k) -> [{k, sub}]
        k when is_atom(k) -> [{k, []}]
        _ -> []
      end)
      |> Map.new()

    Enum.flat_map(indexed_fields, fn {{name, config}, index} ->
      cond do
        Map.has_key?(explicit_keys, name) ->
          [{index, name, Map.fetch!(explicit_keys, name)}]

        loadable_inner?(config) ->
          [{index, name, []}]

        true ->
          []
      end
    end)
  end

  defp loadable_inner?(config) do
    type = config[:type]
    constraints = config[:constraints] || []
    type && Ash.Type.can_load?(type, constraints)
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
       message: error_message("Expected %{expected_length} elements, got %{value_length}"),
       expected_length: field_length,
       value_length: value_length}
    end
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  # stored fields are in the embedded format (see `dump_to_native/2`),
  # so they are loaded with `cast_from_embedded`
  def cast_stored(value, constraints) when is_map(value) do
    cast_tuple_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  # fields are dumped with `dump_to_embedded` because the dumped map is
  # JSON-encoded by data layers, and `dump_to_native` may produce values
  # that are not JSON-safe (e.g. raw binaries for `:binary`/`:uuid_v7`)
  def dump_to_native(value, constraints) when is_tuple(value) do
    dump_tuple_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_native(_, _), do: :error

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, constraints) when is_tuple(value) do
    dump_tuple_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_embedded(_, _), do: :error

  @impl true
  def cast_from_embedded(nil, _), do: {:ok, nil}

  def cast_from_embedded(value, constraints) when is_map(value) do
    cast_tuple_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_from_embedded(_, _), do: :error

  defp cast_tuple_fields(value, constraints, cast_fn) do
    Enum.reduce_while(constraints[:fields], {:ok, []}, fn {key, config}, {:ok, acc} ->
      case fetch_field(value, key) do
        {:ok, value} ->
          case cast_fn.(config[:type], value, config[:constraints] || []) do
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

  defp dump_tuple_fields(value, constraints, dump_fn) do
    list = Tuple.to_list(value)

    if length(list) == length(constraints[:fields]) do
      list
      |> Enum.zip(constraints[:fields])
      |> Enum.reduce_while({:ok, %{}}, fn {tuple_val, {key, config}}, {:ok, acc} ->
        case dump_fn.(config[:type], tuple_val, config[:constraints] || []) do
          {:ok, dumped} ->
            {:cont, {:ok, Map.put(acc, key, dumped)}}

          other ->
            {:halt, other}
        end
      end)
    else
      :error
    end
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

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
              {:error, [[message: error_message("value must not be nil"), field: field]]}
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
        {:error, [[message: error_message("invalid value"), field: field]]}
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
