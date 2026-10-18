# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Helpers do
  @moduledoc false

  def handle_indexed_maps({:array, type}, term) when is_map(term) and term != %{} do
    term
    |> Enum.reduce_while({:ok, []}, fn
      {key, value}, {:ok, acc} when is_integer(key) ->
        {:cont, {:ok, [{key, value} | acc]}}

      {key, value}, {:ok, acc} when is_binary(key) ->
        case Integer.parse(key) do
          {int, ""} ->
            {:cont, {:ok, [{int, value} | acc]}}

          _ ->
            {:halt, :error}
        end

      _, _ ->
        {:halt, :error}
    end)
    |> case do
      {:ok, value} ->
        value
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(&elem(&1, 1))
        |> Enum.map(&handle_indexed_maps(type, &1))

      :error ->
        term
    end
  end

  def handle_indexed_maps(_, value), do: value

  def error_to_exception_opts(message, attribute) do
    case message do
      keyword when is_list(keyword) ->
        fields =
          case List.wrap(keyword[:fields]) do
            [] ->
              List.wrap(keyword[:field])

            fields ->
              fields
          end

        fields
        |> case do
          [] ->
            [
              keyword
              |> Keyword.put(
                :message,
                keyword[:message]
              )
              |> Keyword.put(:field, attribute.name)
            ]

          fields ->
            Enum.map(
              fields,
              fn field ->
                keyword
                |> Keyword.put(:field, field)
                |> Keyword.delete(:fields)
                |> Keyword.update(:path, [attribute.name], &[attribute.name | &1])
              end
            )
        end

      message when is_binary(message) ->
        [[field: attribute.name, message: message]]

      value when is_exception(value) ->
        exception =
          value
          |> Ash.Error.to_ash_error()

        if Map.get(exception, :field) || Map.get(exception, :fields) do
          Ash.Error.set_path(exception, attribute.name)
        else
          Map.put(exception, :field, attribute.name)
        end

      _ ->
        [[field: attribute.name]]
    end
  end

  def dump_fields(value, fields, dump_fn, opts \\ []) do
    drop_nil? = Keyword.get(opts, :drop_nil?, false)
    fetch = Keyword.get(opts, :fetch, &fetch_field/2)

    Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
      case fetch.(value, key) do
        {:ok, field_value} ->
          case dump_fn.(config[:type], field_value, config[:constraints] || []) do
            {:ok, nil} when drop_nil? -> {:cont, {:ok, acc}}
            {:ok, dumped} -> {:cont, {:ok, Map.put(acc, key, dumped)}}
            other -> {:halt, other}
          end

        :error ->
          {:cont, {:ok, acc}}
      end
    end)
  end

  def cast_fields(value, fields, cast_fn, acc, opts \\ []) do
    drop_nil? = Keyword.get(opts, :drop_nil?, false)

    Enum.reduce_while(fields, {:ok, acc}, fn {key, config}, {:ok, casted} ->
      case fetch_field(value, key) do
        {:ok, field_value} ->
          case cast_fn.(config[:type], field_value, config[:constraints] || []) do
            {:ok, nil} when drop_nil? -> {:cont, {:ok, casted}}
            {:ok, cast_value} -> {:cont, {:ok, Map.put(casted, key, cast_value)}}
            other -> {:halt, other}
          end

        :error ->
          {:cont, {:ok, casted}}
      end
    end)
  end

  def fetch_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> {:ok, value}
      :error -> fetch_field(map, to_string(atom))
    end
  end

  def fetch_field(map, key), do: Map.fetch(map, key)
end
