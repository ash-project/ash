defmodule Ash.Type.Helpers do
  @moduledoc false

  def cast_input(type, term, constraints, changeset, return_value? \\ false)

  def cast_input(type, value, constraints, changeset, return_value?) do
    value = handle_indexed_maps(type, value)
    constraints = Ash.Type.constraints(changeset, type, constraints)

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
                add_index(keyword[:message], keyword)
              )
              |> Keyword.put(:field, attribute.name)
            ]

          fields ->
            Enum.map(
              fields,
              &Keyword.merge(message,
                field: attribute.name,
                message: add_index(add_field(keyword[:message], "#{&1}"), keyword)
              )
            )
        end

      message when is_binary(message) ->
        [[field: attribute.name, message: message]]

      value when is_exception(value) ->
        value
        |> Ash.Error.to_ash_error()
        |> Map.put(:field, attribute.name)

      _ ->
        [[field: attribute.name]]
    end
  end

  defp add_field(message, field) do
    "at field #{field} " <> (message || "")
  end

  defp add_index(message, opts) do
    if opts[:index] do
      "at index #{opts[:index]} " <> (message || "")
    else
      message
    end
  end
end
