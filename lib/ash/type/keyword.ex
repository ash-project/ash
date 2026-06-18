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
  import Ash.Gettext

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def matches_type?(v, _constraints) do
    Keyword.keyword?(v)
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
      kw, {:ok, acc} when is_list(kw) ->
        case Keyword.get(kw, key) do
          nil ->
            {:cont, {:ok, [kw | acc]}}

          list when is_list(list) ->
            case Ash.Type.load(inner_type, list, sub_load, inner_constraints, context) do
              {:ok, loaded} -> {:cont, {:ok, [Keyword.put(kw, key, loaded) | acc]}}
              {:error, error} -> {:halt, {:error, error}}
            end

          other ->
            {:cont, {:ok, [Keyword.put(kw, key, other) | acc]}}
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
        kw when is_list(kw) -> Keyword.get(kw, key)
        _ -> nil
      end)

    case Ash.Type.load(inner_type, slice, sub_load, inner_constraints, context) do
      {:ok, loaded_slice} ->
        updated =
          values
          |> Enum.zip(loaded_slice)
          |> Enum.map(fn
            {kw, v} when is_list(kw) -> Keyword.put(kw, key, v)
            {other, _v} -> other
          end)

        {:cont, {:ok, updated}}

      {:error, error} ->
        {:halt, {:error, error}}
    end
  end

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
               field_constraints[:constraints]
             ) do
          {:ok, nil} ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: error_message("value must not be nil"), field: field]]}
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
        {:error, [[message: error_message("invalid value"), field: field]]}
    end
  end

  defp fetch_field(keyword, atom) when is_atom(atom) do
    Keyword.fetch(keyword, atom)
  end

  defp fetch_field(keyword, string) when is_binary(string) do
    fetch_field(keyword, String.to_existing_atom(string))
  end

  defp fetch_field(_, _), do: :error

  defp cast_keyword_fields(value, constraints, cast_fn) do
    if fields = constraints[:fields] do
      fields
      |> Enum.reduce_while({:ok, []}, fn {key, config}, {:ok, acc} ->
        case cast_keyword_field(value, key, config, cast_fn) do
          {:ok, value} -> {:cont, {:ok, [{key, value} | acc]}}
          :skip -> {:cont, {:ok, acc}}
          other -> {:halt, other}
        end
      end)
      |> case do
        {:ok, keyword} -> {:ok, Enum.reverse(keyword)}
        other -> other
      end
    else
      {:ok, try_map_to_keyword(value, constraints)}
    end
  end

  defp cast_keyword_field(map, key, config, cast_fn) do
    case fetch_map_field(map, key) do
      {:ok, value} -> cast_fn.(config[:type], value, config[:constraints] || [])
      :error -> :skip
    end
  end

  defp dump_fields(value, constraints, dump_fn) do
    if fields = constraints[:fields] do
      Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
        case dump_field(value, key, config, dump_fn) do
          {:ok, dumped} -> {:cont, {:ok, Map.put(acc, key, dumped)}}
          :skip -> {:cont, {:ok, acc}}
          other -> {:halt, other}
        end
      end)
    else
      {:ok, if(is_map(value), do: value, else: Map.new(value))}
    end
  end

  defp dump_field(value, key, config, dump_fn) do
    fetched =
      if is_map(value),
        do: fetch_map_field(value, key),
        else: Keyword.fetch(value, key)

    case fetched do
      {:ok, field_value} -> dump_fn.(config[:type], field_value, config[:constraints] || [])
      :error -> :skip
    end
  end

  defp fetch_map_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> {:ok, value}
      :error -> Map.fetch(map, to_string(atom))
    end
  end

  defp try_map_to_keyword(map, constraints) do
    constraints[:fields]
    |> Kernel.||([])
    |> Enum.flat_map(fn {key, field_constraints} ->
      with :error <- Map.fetch(map, key),
           :error <- Map.fetch(map, to_string(key)) do
        []
      else
        {:ok, value} ->
          if type = field_constraints[:type] do
            case Ash.Type.cast_stored(type, value, field_constraints[:constraints] || []) do
              {:ok, cast_value} -> [{key, cast_value}]
              _ -> []
            end
          else
            [{key, value}]
          end
      end
    end)
  end
end
