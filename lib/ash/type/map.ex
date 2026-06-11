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
  import Ash.Gettext

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

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
  def matches_type?(v, _constraints) do
    is_map(v)
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

    fields
    |> effective_load(load)
    |> Enum.reduce_while({:ok, values}, fn {key, sub_load}, {:ok, values} ->
      config = Keyword.fetch!(fields, key)
      inner_type = config[:type]
      inner_constraints = config[:constraints] || []

      load_field_across(values, key, inner_type, inner_constraints, sub_load, context)
    end)
  end

  # For non-array inner types we batch the slice across all records and
  # let the inner type's load callback see the whole batch at once. For
  # `{:array, _}` fields the value at this key is already a list per
  # record, so we have to iterate per-record — otherwise the dispatcher's
  # `splicing_nil_values` would flatten the per-record arrays into one
  # and lose record grouping.
  defp load_field_across(
         values,
         key,
         {:array, _} = inner_type,
         inner_constraints,
         sub_load,
         context
       ) do
    values
    |> Enum.reduce_while({:ok, []}, fn
      %{} = map, {:ok, acc} ->
        case Map.get(map, key) do
          nil ->
            {:cont, {:ok, [map | acc]}}

          list when is_list(list) ->
            case Ash.Type.load(inner_type, list, sub_load, inner_constraints, context) do
              {:ok, loaded} -> {:cont, {:ok, [Map.put(map, key, loaded) | acc]}}
              {:error, error} -> {:halt, {:error, error}}
            end

          other ->
            {:cont, {:ok, [Map.put(map, key, other) | acc]}}
        end

      other, {:ok, acc} ->
        {:cont, {:ok, [other | acc]}}
    end)
    |> case do
      {:ok, reversed} -> {:cont, {:ok, Enum.reverse(reversed)}}
      {:error, error} -> {:halt, {:error, error}}
    end
  end

  defp load_field_across(values, key, inner_type, inner_constraints, sub_load, context) do
    slice =
      Enum.map(values, fn
        %{} = v -> Map.get(v, key)
        _ -> nil
      end)

    case Ash.Type.load(inner_type, slice, sub_load, inner_constraints, context) do
      {:ok, loaded_slice} ->
        updated =
          values
          |> Enum.zip(loaded_slice)
          |> Enum.map(fn
            {%{} = m, v} -> Map.put(m, key, v)
            {other, _v} -> other
          end)

        {:cont, {:ok, updated}}

      {:error, error} ->
        {:halt, {:error, error}}
    end
  end

  # Caller's explicit sub-loads union with auto-recursion into any
  # declared field whose type is loadable. The auto-added sub-load is
  # `[]` — telling inner load callbacks to do their default work
  # (e.g. `Ash.Type.Struct.load/4` applies field-policy scrubbing on
  # populated values).
  defp effective_load(fields, load) do
    explicit =
      load
      |> List.wrap()
      |> Enum.flat_map(fn
        {k, sub} when is_atom(k) -> [{k, sub}]
        k when is_atom(k) -> [{k, []}]
        _ -> []
      end)
      |> Map.new()

    Enum.reduce(fields, explicit, fn {field_name, config}, acc ->
      cond do
        Map.has_key?(acc, field_name) ->
          acc

        loadable_inner?(config) ->
          Map.put(acc, field_name, [])

        true ->
          acc
      end
    end)
  end

  defp loadable_inner?(config) do
    type = config[:type]
    constraints = config[:constraints] || []
    type && Ash.Type.can_load?(type, constraints)
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
    cast_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
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

  def dump_to_native(_, _), do: :error

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, constraints) when is_map(value) do
    dump_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_embedded(_, _), do: :error

  @impl true
  def cast_from_embedded(nil, _), do: {:ok, nil}

  def cast_from_embedded(value, constraints) when is_map(value) do
    cast_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_from_embedded(_, _), do: :error

  defp dump_fields(value, constraints, dump_fn) do
    if fields = constraints[:fields] do
      nil_values = constraints[:preserve_nil_values?] == true

      Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
        case dump_field(value, key, config, dump_fn) do
          {:ok, nil} when not nil_values -> {:cont, {:ok, acc}}
          {:ok, dumped} -> {:cont, {:ok, Map.put(acc, key, dumped)}}
          :skip -> {:cont, {:ok, acc}}
          other -> {:halt, other}
        end
      end)
    else
      {:ok, value}
    end
  end

  defp dump_field(value, key, config, dump_fn) do
    case fetch_field(value, key) do
      {:ok, field_value} -> dump_fn.(config[:type], field_value, config[:constraints] || [])
      :error -> :skip
    end
  end

  defp cast_fields(value, constraints, cast_fn) do
    if fields = constraints[:fields] do
      nil_values = constraints[:preserve_nil_values?] == true

      Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
        case fetch_field(value, key) do
          {:ok, value} ->
            case cast_fn.(config[:type], value, config[:constraints] || []) do
              {:ok, value} ->
                if is_nil(value) && !nil_values do
                  {:cont, {:ok, acc}}
                else
                  {:cont, {:ok, Map.put(acc, key, value)}}
                end

              other ->
                {:halt, other}
            end

          :error ->
            {:cont, {:ok, acc}}
        end
      end)
    else
      {:ok, value}
    end
  end

  @impl true
  def apply_constraints(nil, _constraints), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    Enum.reduce(constraints, {:ok, value}, fn
      {:fields, fields}, {:ok, value} ->
        check_fields(value, fields)

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

  defp check_fields(value, fields) do
    {errors, result} =
      Enum.reduce(fields, {[], %{}}, fn {field, field_constraints}, {errors_acc, result_acc} ->
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
              field_error = [message: error_message("field must be present"), field: field]
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
               field_constraints[:constraints] || []
             ) do
          {:ok, nil} ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: error_message("value must not be nil"), field: field]]}
            else
              {:ok, Map.put(result, field, nil)}
            end

          {:ok, field_value} ->
            {:ok, Map.put(result, field, field_value)}

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
