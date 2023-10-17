defmodule Ash.Type.Helpers do
  @moduledoc false

  def cast_input(type, term, constraints, changeset, return_value? \\ false)

  def cast_input(type, value, constraints, changeset, return_value?) do
    value = handle_indexed_maps(type, value)
    constraints = Ash.Type.include_source(type, changeset, constraints)

    case Ash.Type.cast_input(type, value, constraints) do
      {:ok, value} ->
        {:ok, value}

      {:error, error} ->
        if return_value? do
          {{:error, error}, value}
        else
          {:error, error}
        end
    end
  end

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
end
