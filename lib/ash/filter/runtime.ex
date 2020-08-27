defmodule Ash.Filter.Runtime do
  @moduledoc """
  Checks a record to see if it matches a filter statement.

  We can't always tell if a record matches a filter statement, and as such
  this function may return `:unknown`
  """
  alias Ash.Filter.{Expression, Not, Predicate}

  def matches?(api, record, filter, dirty_fields \\ []) do
    case do_matches?(record, filter, dirty_fields) do
      {:ok, boolean} ->
        boolean

      {:side_load, side_loads} ->
        case do_matches?(api.side_load!(record, side_loads), filter, dirty_fields) do
          {:ok, boolean} -> boolean
          _ -> false
        end
    end
  end

  defp do_matches?(record, filter, dirty_fields, side_loads \\ []) do
    case filter.expression do
      nil ->
        {:ok, true}

      %Predicate{predicate: predicate, relationship_path: [], attribute: attribute} ->
        if attribute.name in dirty_fields do
          {:ok, :unknown}
        else
          {:ok, Predicate.match?(predicate, Map.get(record, attribute.name), attribute.type)}
        end

      %Predicate{predicate: predicate, attribute: attribute, relationship_path: relationship_path} ->
        side_load_predicate_matches(record, relationship_path, predicate, attribute, side_loads)

      %Not{expression: expression} ->
        case do_matches?(record, expression, dirty_fields, side_loads) do
          {:ok, :unknown} ->
            {:ok, :unknown}

          {:ok, match?} ->
            {:ok, !match?}

          {:side_load, side_loads} ->
            {:side_load, side_loads}
        end

      %Expression{op: op, left: left, right: right} ->
        expression_matches(op, left, right, record, dirty_fields, side_loads)
    end
  end

  defp side_load_predicate_matches(record, relationship_path, predicate, attribute, side_loads) do
    if loaded?(record, relationship_path) do
      records = get_related(record, relationship_path)

      Enum.reduce_while(records, {:ok, false}, fn record, {:ok, status} ->
        case Predicate.match?(predicate, Map.get(record, attribute.name), attribute.type) do
          :unknown ->
            if status == false do
              {:ok, :unknown}
            else
              {:ok, status}
            end

          true ->
            {:halt, {:ok, true}}

          false ->
            {:ok, false}
        end
      end)
    else
      {:side_load, [relationship_path | side_loads]}
    end
  end

  defp expression_matches(:and, left, right, record, dirty_fields, side_loads) do
    case do_matches?(record, left, dirty_fields, side_loads) do
      {:ok, true} ->
        do_matches?(record, right, dirty_fields, side_loads)

      {:ok, :unknown} ->
        {:ok, :unknown}

      {:ok, false} ->
        {:ok, false}

      {:side_load, side_loads} ->
        do_matches?(record, right, dirty_fields, side_loads)
    end
  end

  defp expression_matches(:or, left, right, record, dirty_fields, side_loads) do
    case do_matches?(record, left, dirty_fields, side_loads) do
      {:ok, true} ->
        {:ok, true}

      {:ok, :unknown} ->
        case do_matches?(record, right, dirty_fields, side_loads) do
          {:ok, false} -> {:ok, :unknown}
          other -> other
        end

      {:ok, false} ->
        do_matches?(record, right, dirty_fields, side_loads)

      {:side_load, side_loads} ->
        do_matches?(record, right, dirty_fields, side_loads)
    end
  end

  defp get_related(record, path) when not is_list(record) do
    get_related([record], path)
  end

  defp get_related(records, []) do
    records
  end

  defp get_related(records, [key | rest]) when is_list(records) do
    Enum.flat_map(records, fn record ->
      case Map.get(record, key) do
        %Ash.NotLoaded{type: :relationship} ->
          []

        value ->
          get_related(value, rest)
      end
    end)
  end

  defp loaded?(records, path) when is_list(records) do
    Enum.all?(records, &loaded?(&1, path))
  end

  defp loaded?(%Ash.NotLoaded{}, _), do: false

  defp loaded?(_, []), do: true

  defp loaded?(record, [key | rest]) do
    record
    |> Map.get(key)
    |> loaded?(rest)
  end
end
