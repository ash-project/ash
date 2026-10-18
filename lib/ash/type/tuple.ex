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
  def acts_as(_), do: :map

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

  @impl true
  def init(constraints) do
    constraints
    |> Keyword.put(:fields, List.wrap(constraints[:fields]))
    |> Ash.Type.CompositeTypeHelpers.init_fields()
  end

  @impl true
  def matches_type?(v, _constraints) do
    is_tuple(v)
  end

  @impl true
  def can_load?(constraints), do: Ash.Type.CompositeTypeHelpers.can_load_fields?(constraints)

  @impl true
  def load(values, load, constraints, context) when is_list(values) do
    Ash.Type.CompositeTypeHelpers.load_fields(
      values,
      load,
      constraints[:fields] || [],
      context,
      %{
        get: fn
          tuple, {_key, index} when is_tuple(tuple) and tuple_size(tuple) > index ->
            {:ok, elem(tuple, index)}

          _, _ ->
            :error
        end,
        put: fn tuple, {_key, index}, value -> put_elem(tuple, index, value) end
      }
    )
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_tuple(value), do: check_size(value, constraints)

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

  # A missing key loads as `nil` in its position, so later fields keep their
  # positions. Loading does not apply constraints, so a missing
  # `allow_nil?: false` field loads as `nil` without an error, the same as a
  # stored `nil`.
  defp cast_tuple_fields(value, constraints, cast_fn) do
    Ash.Type.CompositeTypeHelpers.map_fields(
      value,
      constraints[:fields],
      cast_fn,
      [missing: :fill_nil] ++ tuple_shape()
    )
  end

  defp dump_tuple_fields(value, constraints, dump_fn) do
    fields = constraints[:fields]

    if tuple_size(value) == length(fields) do
      value
      |> Ash.Type.CompositeTypeHelpers.tuple_to_field_map(fields)
      |> Ash.Type.CompositeTypeHelpers.map_fields(fields, dump_fn, fetch: &Map.fetch/2)
    else
      :error
    end
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    fields = constraints[:fields]

    with {:ok, value} <- check_size(value, constraints) do
      value
      |> Ash.Type.CompositeTypeHelpers.tuple_to_field_map(fields)
      |> Ash.Type.CompositeTypeHelpers.check_fields(
        fields,
        [fetch: &Map.fetch/2] ++ tuple_shape()
      )
    end
  end

  # Builds tuple results in field order.
  defp tuple_shape do
    [
      acc: [],
      put: fn acc, _key, value -> [value | acc] end,
      finish: &(&1 |> Enum.reverse() |> List.to_tuple())
    ]
  end

  defp check_size(value, constraints) do
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
end
