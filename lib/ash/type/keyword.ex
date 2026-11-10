# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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
            ],
            init?: [
              type: :boolean,
              default: true,
              doc: """
              If false, the field's type constraints are not initialised at compile time. \
              Allows for recursive keyword fields.
              """
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

  A builtin type that can be referenced via `:keyword`

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def acts_as(_), do: :map

  @impl true
  def matches_type?(v, _constraints) do
    Keyword.keyword?(v)
  end

  @impl true
  def can_load?(constraints), do: Ash.Type.CompositeTypeHelpers.can_load_fields?(constraints)

  # Override the default splice — without this, `splicing_nil_values/2`'s
  # flat_map would tear each keyword-list value apart into its
  # `{key, value}` entries before the load callback ever sees the
  # structured value.
  @impl true
  def splice_nil_values(values, callback) when is_list(values) do
    values
    |> Stream.with_index()
    |> Enum.reduce({[], []}, fn
      {nil, index}, {acc, nil_indices} -> {acc, [index | nil_indices]}
      {value, _index}, {acc, nil_indices} -> {[value | acc], nil_indices}
    end)
    |> then(fn {list, nil_indices} ->
      case callback.(Enum.reverse(list)) do
        {:ok, new_list} ->
          {:ok, Enum.reduce(Enum.reverse(nil_indices), new_list, &List.insert_at(&2, &1, nil))}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  def splice_nil_values(value, callback), do: callback.(value)

  @impl true
  def load(values, load, constraints, context) when is_list(values) do
    Ash.Type.CompositeTypeHelpers.load_fields(
      values,
      load,
      constraints[:fields] || [],
      context,
      %{
        get: fn
          kw, {key, _index} when is_list(kw) -> Keyword.fetch(kw, key)
          _, _ -> :error
        end,
        put: fn kw, {key, _index}, value -> List.keyreplace(kw, key, 0, {key, value}) end
      }
    )
  end

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

  @impl true
  def init(constraints), do: Ash.Type.CompositeTypeHelpers.init_fields(constraints)

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

  # stored fields are in the embedded format (see `dump_to_native/2`),
  # so they are loaded with `cast_from_embedded`
  def cast_stored(value, constraints) when is_map(value) do
    cast_keyword_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  # fields are dumped with `dump_to_embedded` because the dumped map is
  # JSON-encoded by data layers, and `dump_to_native` may produce values
  # that are not JSON-safe (e.g. raw binaries for `:binary`/`:uuid_v7`)
  def dump_to_native(value, constraints) when is_map(value) do
    dump_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_native(value, constraints) do
    if Keyword.keyword?(value) do
      dump_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
    else
      :error
    end
  end

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, constraints) when is_map(value) do
    dump_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_embedded(value, constraints) do
    if Keyword.keyword?(value) do
      dump_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
    else
      :error
    end
  end

  @impl true
  def cast_from_embedded(nil, _), do: {:ok, nil}

  def cast_from_embedded(value, constraints) when is_map(value) do
    cast_keyword_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_from_embedded(_, _), do: :error

  @impl true
  def apply_constraints(value, constraints) do
    Enum.reduce(constraints, {:ok, value}, fn
      {:fields, fields}, {:ok, value} ->
        Ash.Type.CompositeTypeHelpers.check_fields(
          value,
          fields,
          [fetch: &Keyword.fetch/2] ++ keyword_shape()
        )

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

  defp cast_keyword_fields(value, constraints, cast_fn) do
    if fields = constraints[:fields] do
      Ash.Type.CompositeTypeHelpers.map_fields(value, fields, cast_fn, keyword_shape())
    else
      {:ok, try_map_to_keyword(value, constraints)}
    end
  end

  defp dump_fields(value, constraints, dump_fn) do
    if fields = constraints[:fields] do
      Ash.Type.CompositeTypeHelpers.map_fields(value, fields, dump_fn, fetch: &fetch_dump_field/2)
    else
      {:ok, if(is_map(value), do: value, else: Map.new(value))}
    end
  end

  # Builds keyword results in field order.
  defp keyword_shape do
    [acc: [], put: fn acc, key, value -> [{key, value} | acc] end, finish: &Enum.reverse/1]
  end

  defp fetch_dump_field(value, key) when is_map(value),
    do: Ash.Type.CompositeTypeHelpers.fetch_field(value, key)

  defp fetch_dump_field(value, key), do: Keyword.fetch(value, key)

  defp try_map_to_keyword(map, constraints) do
    constraints[:fields]
    |> Kernel.||([])
    |> Enum.flat_map(fn {key, field_constraints} ->
      case Ash.Type.CompositeTypeHelpers.fetch_field(map, key) do
        {:ok, value} ->
          if type = field_constraints[:type] do
            case Ash.Type.cast_stored(type, value, field_constraints[:constraints] || []) do
              {:ok, cast_value} -> [{key, cast_value}]
              _ -> []
            end
          else
            [{key, value}]
          end

        :error ->
          []
      end
    end)
  end
end
