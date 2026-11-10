# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.CompositeTypeHelpers do
  @moduledoc """
  Shared utilities for the composite types: maps, keyword lists, structs,
  tuples and typed structs.

  Typed structs use this module to convert their validation errors. The other
  documented error formatters are no longer used by the composite types and
  remain for compatibility. The functions that cast, dump, load and check
  declared fields are internal.
  """

  import Ash.Gettext

  @doc """
  Converts validation errors from composite types to InvalidAttribute exceptions.

  Handles multiple error formats:
  - Single keyword list errors
  - Lists of keyword list errors and exceptions; exceptions pass through
  - Other error formats (passed through to Ash.Error.to_ash_error)

  Keyword list errors keep their `:path`.

  For backward compatibility with typed_struct.ex, returns a single error
  for single error lists, and multiple errors for multiple error lists.
  """
  def convert_errors_to_invalid_attributes(error) do
    cond do
      Keyword.keyword?(error) ->
        {:error, keyword_to_invalid_attribute(error)}

      is_list(error) && Enum.all?(error, &(Keyword.keyword?(&1) or is_exception(&1))) ->
        errors =
          Enum.map(error, fn
            err when is_exception(err) -> err
            err -> keyword_to_invalid_attribute(err)
          end)

        case errors do
          [single_error] -> {:error, single_error}
          multiple_errors -> {:error, multiple_errors}
        end

      true ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp keyword_to_invalid_attribute(error) do
    Ash.Error.Changes.InvalidAttribute.exception(
      field: error[:field],
      message: error[:message],
      value: error[:value],
      path: error[:path] || [],
      vars: error
    )
  end

  @doc """
  Converts constraint errors to keyword list format, ensuring each error
  has a :field key set to the specified field name.
  """
  def convert_constraint_errors_to_keyword_lists(errors, field) do
    errors =
      if Keyword.keyword?(errors) do
        [errors]
      else
        List.wrap(errors)
      end

    Enum.map(errors, &format_error_as_keyword(&1, field))
  end

  @doc """
  Formats a single constraint error as a comprehensive keyword list.

  Keeps the error's own `:field` if it has one and sets `field` otherwise.
  """
  def format_comprehensive_error_as_keyword(error, field) do
    case error do
      [_ | _] = keyword_list when is_list(keyword_list) ->
        if Keyword.has_key?(keyword_list, :field) do
          keyword_list
        else
          Keyword.put(keyword_list, :field, field)
        end

      {key, value} when key != :message ->
        [{key, value}, {:field, field}]

      {:message, message} ->
        [message: message, field: field]

      binary when is_binary(binary) ->
        [message: binary, field: field]

      other ->
        [message: inspect(other), field: field]
    end
  end

  @doc """
  Filters out non-informative constraint errors.

  Removes constraint key-value pairs that don't provide meaningful
  error messages to users.
  """
  def filter_non_informative_errors(errors) do
    Enum.reject(errors, fn error ->
      case error do
        [{key, _value}, {:field, _field}]
        when key in [:min, :max, :regex, :min_length, :max_length] ->
          true

        _ ->
          false
      end
    end)
  end

  defp format_error_as_keyword(error, field) do
    cond do
      Keyword.keyword?(error) ->
        Keyword.put(error, :field, field)

      is_binary(error) ->
        [field: field, message: error]

      is_tuple(error) ->
        [field: field, message: inspect(error)]

      true ->
        [field: field, message: to_string(error)]
    end
  end

  # Field engine for the composite types: maps, keyword lists, structs and
  # tuples. These functions walk the declared fields, and each type describes
  # its in-memory shape.
  #
  # Loading takes an access map that reads and writes one field slot of a
  # value, addressed by field name and position:
  #
  #   * `:get` - returns `{:ok, field_value}`, or `:error` when the value has
  #     no such slot. Loading passes such values through unchanged.
  #   * `:put` - returns the value with the slot set.
  #
  # Casting, dumping and checking take the shape as options:
  #
  #   * `:fetch` - reads a field from the input, returning `{:ok, value}` or
  #     `:error`. Defaults to reading the atom key, then its string form.
  #   * `:acc`, `:put` and `:finish` - build the result. Default to an empty
  #     map, putting each field into it, and returning it unchanged.
  #
  # All four types share one field-error format.

  # Initializes the type and constraints of every declared field.
  @doc false
  # Rejects duplicate field names: storage keys fields by name, so duplicates
  # could never round-trip.
  def init_fields(constraints) do
    case constraints[:fields] do
      fields when is_list(fields) ->
        case duplicate_names(fields) do
          [] ->
            init_each_field(constraints, fields)

          duplicates ->
            {:error, "fields must have unique names, got duplicates `#{inspect(duplicates)}`"}
        end

      nil ->
        {:ok, constraints}

      fields ->
        {:error, "fields must be a list, got `#{inspect(fields)}`"}
    end
  end

  defp duplicate_names(fields) do
    names = Keyword.keys(fields)
    Enum.uniq(names -- Enum.uniq(names))
  end

  defp init_each_field(constraints, fields) do
    fields
    |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, fields} ->
      case init_field(config) do
        {:ok, config} -> {:cont, {:ok, [{name, config} | fields]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, fields} -> {:ok, Keyword.put(constraints, :fields, Enum.reverse(fields))}
      {:error, error} -> {:error, error}
    end
  end

  defp init_field(config) do
    type = Ash.Type.get_type(config[:type])

    if Keyword.get(config, :init?, true) do
      case Ash.Type.init(type, config[:constraints] || []) do
        {:ok, constraints} -> {:ok, Keyword.merge(config, constraints: constraints, type: type)}
        {:error, error} -> {:error, error}
      end
    else
      {:ok, config}
    end
  end

  # Returns true if any declared field has a loadable type.
  @doc false
  def can_load_fields?(constraints) do
    case constraints[:fields] do
      fields when is_list(fields) ->
        Enum.any?(fields, fn {_name, config} -> loadable?(config) end)

      _ ->
        false
    end
  end

  defp loadable?(config) do
    type = config[:type]
    type && Ash.Type.can_load?(type, config[:constraints] || [])
  end

  # Loads the declared fields across a batch of composite values through the
  # access map described above.
  @doc false
  def load_fields(values, load, fields, context, access) do
    with {:ok, loads} <-
           fields |> Enum.with_index() |> effective_load(load) do
      Enum.reduce_while(loads, {:ok, values}, fn {{name, config}, index, sub_load},
                                                 {:ok, values} ->
        case load_slot(values, {name, index}, config, sub_load, context, access) do
          {:ok, values} -> {:cont, {:ok, values}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)
    end
  end

  # The caller's explicit sub-loads union with auto-recursion into any
  # declared field whose type is loadable. The auto-added sub-load is `[]`,
  # which tells inner load callbacks to do their default work (e.g. struct
  # fields apply field-policy scrubbing to populated values). An explicit load
  # of an undeclared field returns an error, so a misspelled load fails instead
  # of returning unloaded data.
  defp effective_load(indexed_fields, load) do
    explicit =
      load
      |> List.wrap()
      |> Enum.flat_map(fn
        {k, sub} when is_atom(k) -> [{k, sub}]
        k when is_atom(k) -> [{k, []}]
        _ -> []
      end)
      |> Map.new()

    declared = Enum.map(indexed_fields, fn {{name, _config}, _index} -> name end)

    case Map.keys(explicit) -- declared do
      [] ->
        {:ok,
         Enum.flat_map(indexed_fields, fn {{name, config} = field, index} ->
           cond do
             Map.has_key?(explicit, name) -> [{field, index, Map.fetch!(explicit, name)}]
             loadable?(config) -> [{field, index, []}]
             true -> []
           end
         end)}

      unknown ->
        {:error,
         "cannot load #{inspect(unknown)}: not declared fields. " <>
           "Declared fields: #{inspect(declared)}"}
    end
  end

  # For array fields the slot already holds a list per value, so the loader
  # iterates per value. Batching would let the load dispatcher flatten the
  # per-value lists into one and lose the grouping.
  defp load_slot(values, slot, config, sub_load, context, access) do
    case config[:type] do
      {:array, _} -> load_slot_per_value(values, slot, config, sub_load, context, access)
      _ -> load_slot_batched(values, slot, config, sub_load, context, access)
    end
  end

  defp load_slot_per_value(values, slot, config, sub_load, context, access) do
    values
    |> Enum.reduce_while({:ok, []}, fn value, {:ok, acc} ->
      case access.get.(value, slot) do
        {:ok, nil} ->
          {:cont, {:ok, [value | acc]}}

        {:ok, list} when is_list(list) ->
          case Ash.Type.load(config[:type], list, sub_load, config[:constraints] || [], context) do
            {:ok, loaded} -> {:cont, {:ok, [access.put.(value, slot, loaded) | acc]}}
            {:error, error} -> {:halt, {:error, error}}
          end

        _ ->
          {:cont, {:ok, [value | acc]}}
      end
    end)
    |> case do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      {:error, error} -> {:error, error}
    end
  end

  defp load_slot_batched(values, slot, config, sub_load, context, access) do
    fetched = Enum.map(values, &access.get.(&1, slot))

    slice =
      Enum.map(fetched, fn
        {:ok, field_value} -> field_value
        :error -> nil
      end)

    case Ash.Type.load(config[:type], slice, sub_load, config[:constraints] || [], context) do
      {:ok, loaded_slice} ->
        {:ok,
         [values, fetched, loaded_slice]
         |> Enum.zip_with(fn
           [value, {:ok, _}, loaded] -> access.put.(value, slot, loaded)
           [value, :error, _] -> value
         end)}

      {:error, error} ->
        {:error, error}
    end
  end

  # Applies `fun` (a cast or dump function) to every field present in `value`.
  #
  # Besides the shape options, accepts:
  #
  #   * `:drop_nil?` - omits fields whose result is `nil`. Defaults to `false`.
  #   * `:missing` - `:skip` omits missing fields; `:fill_nil` puts `nil` in
  #     their place. Defaults to `:skip`.
  @doc false
  def map_fields(value, fields, fun, opts \\ []) do
    fetch = Keyword.get(opts, :fetch, &fetch_field/2)
    put = Keyword.get(opts, :put, &Map.put/3)
    drop_nil? = Keyword.get(opts, :drop_nil?, false)
    missing = Keyword.get(opts, :missing, :skip)

    fields
    |> Enum.reduce_while({:ok, Keyword.get(opts, :acc, %{})}, fn {key, config}, {:ok, acc} ->
      case fetch.(value, key) do
        {:ok, field_value} ->
          case fun.(config[:type], field_value, config[:constraints] || []) do
            {:ok, nil} when drop_nil? -> {:cont, {:ok, acc}}
            {:ok, result} -> {:cont, {:ok, put.(acc, key, result)}}
            other -> {:halt, other}
          end

        :error when missing == :fill_nil ->
          {:cont, {:ok, put.(acc, key, nil)}}

        :error ->
          {:cont, {:ok, acc}}
      end
    end)
    |> finish(opts)
  end

  # Casts every declared field as input and applies its constraints, collecting
  # the errors of all fields.
  #
  # A missing field is an error only when the field sets `allow_nil?: false`.
  @doc false
  def check_fields(value, fields, opts \\ []) do
    fetch = Keyword.get(opts, :fetch, &fetch_field/2)
    put = Keyword.get(opts, :put, &Map.put/3)

    {errors, result} =
      Enum.reduce(fields, {[], Keyword.get(opts, :acc, %{})}, fn {field, config},
                                                                 {errors, result} ->
        case fetch.(value, field) do
          {:ok, field_value} ->
            case check_field(field, field_value, config) do
              {:ok, checked} -> {errors, put.(result, field, checked)}
              {:error, field_errors} -> {[field_errors | errors], result}
            end

          :error ->
            if config[:allow_nil?] == false do
              {[[[message: error_message("field must be present"), field: field]] | errors],
               result}
            else
              {errors, result}
            end
        end
      end)

    case errors do
      [] -> finish({:ok, result}, opts)
      _ -> {:error, errors |> Enum.reverse() |> Enum.concat()}
    end
  end

  # Casts one field value as input and applies its constraints.
  @doc false
  def check_field(field, value, config) do
    type = config[:type]
    constraints = config[:constraints] || []

    case Ash.Type.cast_input(type, value, constraints) do
      {:ok, cast_value} ->
        case Ash.Type.apply_constraints(type, cast_value, constraints) do
          {:ok, nil} ->
            if config[:allow_nil?] == false do
              {:error, [[message: error_message("value must not be nil"), field: field]]}
            else
              {:ok, nil}
            end

          {:ok, checked} ->
            {:ok, checked}

          {:error, errors} ->
            {:error, format_error(errors, field, cast_value)}
        end

      {:error, error} ->
        {:error, format_error(error, field, value)}

      # The cast dispatcher turns a bare `:error` into "is invalid", except
      # when a type's own array casting returns one.
      :error ->
        {:error, format_error(error_message("invalid value"), field, value)}
    end
  end

  # Formats a cast or constraint error so that it names the field. The
  # formatter flattens nested lists (e.g. from array fields) and formats each
  # error as follows:
  #
  #   * a string gets `field` and `value`;
  #   * a keyword error or exception that names inner fields (with `:field`
  #     or `:fields`) keeps them and gets `field` prepended to its path;
  #   * one without a field gets `field`;
  #   * an exception becomes an Ash error first;
  #   * any other term becomes the message of an error that names the field.
  defp format_error(error, field, value) do
    error
    |> List.wrap()
    |> Ash.Helpers.flatten_preserving_keywords()
    |> Enum.map(&put_field(&1, field, value))
  end

  defp put_field(message, field, value) when is_binary(message),
    do: [message: message, field: field, value: value]

  defp put_field(error, field, _value) when is_exception(error) do
    error = Ash.Error.to_ash_error(error)

    if Map.has_key?(error, :field) and not names_field?(error.field, Map.get(error, :fields)) do
      %{error | field: field}
    else
      Ash.Error.set_path(error, [field])
    end
  end

  defp put_field(error, field, _value) when is_list(error) do
    if names_field?(error[:field], error[:fields]) do
      Keyword.update(error, :path, [field], &[field | &1])
    else
      Keyword.put(error, :field, field)
    end
  end

  defp put_field(other, field, _value) when is_tuple(other),
    do: [message: inspect(other), field: field]

  defp put_field(other, field, _value) do
    message = if String.Chars.impl_for(other), do: to_string(other), else: inspect(other)
    [message: message, field: field]
  end

  defp names_field?(field, fields), do: not is_nil(field) or fields not in [nil, []]

  # Pairs the elements of a tuple with the field names, in order.
  @doc false
  def tuple_to_field_map(tuple, fields) do
    fields |> Keyword.keys() |> Enum.zip(Tuple.to_list(tuple)) |> Map.new()
  end

  # Fetches an atom key, falling back to its string form.
  @doc false
  def fetch_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> {:ok, value}
      :error -> fetch_field(map, to_string(atom))
    end
  end

  def fetch_field(map, key), do: Map.fetch(map, key)

  defp finish({:ok, acc}, opts), do: {:ok, Keyword.get(opts, :finish, & &1).(acc)}
  defp finish(other, _opts), do: other
end
