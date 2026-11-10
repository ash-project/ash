# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Map do
  @constraints [
    preserve_nil_values?: [
      type: :boolean,
      default: false,
      doc: """
      If set to true, nil values will be preserved both when storing and in the casted map.
      Otherwise, keys whose values are `nil` will be omitted.

      preserved_nil_values? is false by default
      """
    ],
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
              Allows for recursive map fields.
              """
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
  Represents a map stored in the database.

  In postgres, for example, this represents binary encoded json

  A builtin type that can be referenced via `:map`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

  @impl true
  def init(constraints), do: Ash.Type.CompositeTypeHelpers.init_fields(constraints)

  @impl true
  def matches_type?(v, _constraints) do
    is_map(v)
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
          %{} = map, {key, _index} -> Map.fetch(map, key)
          _, _ -> :error
        end,
        put: fn map, {key, _index}, value -> Map.put(map, key, value) end
      }
    )
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

  def cast_input(value, _) when is_map(value), do: {:ok, value}
  def cast_input(_, _), do: :error

  @impl true

  def cast_stored(nil, _), do: {:ok, nil}

  # stored fields are in the embedded format (see `dump_to_native/2`),
  # so they are loaded with `cast_from_embedded`
  def cast_stored(value, constraints) when is_map(value) do
    map_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  # fields are dumped with `dump_to_embedded` because the dumped map is
  # JSON-encoded by data layers, and `dump_to_native` may produce values
  # that are not JSON-safe (e.g. raw binaries for `:binary`/`:uuid_v7`)
  def dump_to_native(value, constraints) when is_map(value) do
    map_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_native(_, _), do: :error

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, constraints) when is_map(value) do
    map_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_embedded(_, _), do: :error

  @impl true
  def cast_from_embedded(nil, _), do: {:ok, nil}

  def cast_from_embedded(value, constraints) when is_map(value) do
    map_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_from_embedded(_, _), do: :error

  # Casts or dumps each declared field with `fun`.
  defp map_fields(value, constraints, fun) do
    if fields = constraints[:fields] do
      Ash.Type.CompositeTypeHelpers.map_fields(value, fields, fun,
        drop_nil?: constraints[:preserve_nil_values?] != true
      )
    else
      {:ok, value}
    end
  end

  @impl true
  def apply_constraints(nil, _constraints), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    Enum.reduce(constraints, {:ok, value}, fn
      {:fields, fields}, {:ok, value} ->
        Ash.Type.CompositeTypeHelpers.check_fields(value, fields)

      {:preserve_nil_values?, _}, errors ->
        errors

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  @impl true
  def generator(constraints) do
    if constraints[:fields] do
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

        generator =
          type
          |> Ash.Type.generator(constraints)
          |> StreamData.filter(fn item ->
            with {:ok, value} <- Ash.Type.cast_input(config[:type], item, config[:constraints]),
                 {:ok, nil} <-
                   Ash.Type.apply_constraints(config[:type], value, config[:constraints]) do
              false
            else
              _ ->
                true
            end
          end)

        {key, generator}
      end)
      |> Ash.Generator.mixed_map(optional)
    else
      StreamData.constant(%{})
    end
  end
end
