# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.CompositeTypeHelpers do
  @moduledoc """
  Shared utilities for composite type validation and error handling.

  This module provides common functions used by composite types like
  TypedStruct, Map, Keyword, and Struct for handling validation errors
  and converting them to consistent formats.
  """

  @doc """
  Converts validation errors from composite types to InvalidAttribute exceptions.

  Handles multiple error formats:
  - Single keyword list errors
  - Lists of keyword list errors
  - Other error formats (passed through to Ash.Error.to_ash_error)

  For backward compatibility with typed_struct.ex, returns a single error
  for single error lists, and multiple errors for multiple error lists.
  """
  def convert_errors_to_invalid_attributes(error) do
    cond do
      Keyword.keyword?(error) ->
        {:error,
         Ash.Error.Changes.InvalidAttribute.exception(
           field: error[:field],
           message: error[:message],
           value: error[:value],
           vars: error
         )}

      is_list(error) && Enum.all?(error, &Keyword.keyword?/1) ->
        errors =
          Enum.map(error, fn err ->
            Ash.Error.Changes.InvalidAttribute.exception(
              field: err[:field],
              message: err[:message],
              value: err[:value],
              vars: err
            )
          end)

        case errors do
          [single_error] -> {:error, single_error}
          multiple_errors -> {:error, multiple_errors}
        end

      true ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  @doc """
  Converts constraint errors to keyword list format, ensuring each error
  has a :field key set to the specified field name.

  This is used by Map, Keyword, and Struct types to normalize error formats
  from nested type validations.
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

  This handles the complex error formatting logic from struct.ex, including
  filtering out non-informative constraint key-value pairs and ensuring
  proper field assignment.
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
end
