defmodule Ash.Filter.Runtime do
  @moduledoc """
  Checks a record to see if it matches a filter statement.

  We can't always tell if a record matches a filter statement, and as such
  this function may return `:unknown`
  """
  alias Ash.Query.{Expression, Not, Ref}

  @doc """
  Checks if a record matches a filter, side loading any necessary relationships"

  If it can't tell, this returns false.
  """
  def matches?(api, record, filter, dirty_fields \\ []) do
    case matches(record, filter, dirty_fields) do
      {:ok, boolean} ->
        boolean

      {:side_load, side_loads} when not is_nil(api) ->
        matches?(api, api.load!(record, side_loads), filter, dirty_fields)

      {:side_load, _} ->
        false

      :unknown ->
        false
    end
  end

  def matches(record, filter, dirty_fields, side_loads \\ [])

  def matches(record, expression, dirty_fields, side_loads) do
    case expression do
      %Ash.Filter{expression: expression} ->
        matches(record, expression, dirty_fields, side_loads)

      nil ->
        {:ok, true}

      boolean when is_boolean(boolean) ->
        {:ok, boolean}

      %op{__operator__?: true, left: left, right: right} = operator ->
        with true <- :erlang.function_exported(op, :match?, 1),
             {:dirty?, false} <- {:dirty?, dirty?([left, right], dirty_fields)},
             {:side_load, []} <- {:side_load, need_to_load([left, right], record)} do
          case right do
            %Ref{} ->
              {:ok,
               right
               |> resolve_ref(record)
               |> List.wrap()
               |> Enum.any?(fn right_resolved ->
                 left
                 |> resolve_ref(record)
                 |> List.wrap()
                 |> Enum.any?(fn left_resolved ->
                   op.evaluate(%{operator | left: left_resolved, right: right_resolved})
                 end)
               end)}

            _ ->
              {:ok,
               left
               |> resolve_ref(record)
               |> Enum.any?(fn left_resolved ->
                 op.evaluate(%{operator | left: left_resolved, right: right})
               end)}
          end
        else
          false ->
            :unknown

          {:side_load, paths} ->
            {:side_load, paths}

          {:dirty?, true} ->
            :unknown
        end

      %func{__function__?: true, arguments: arguments} = function ->
        with true <- :erlang.function_exported(func, :match?, 1),
             {:dirty?, false} <- {:dirty?, dirty?(arguments, dirty_fields)},
             {:side_load, []} <- {:side_load, need_to_load(arguments, record)} do
          {:ok,
           arguments
           |> Enum.map(&resolve_ref(&1, record))
           |> unique_calls()
           |> Enum.any?(fn args ->
             func.evaluate(%{function | arguments: args})
           end)}
        else
          false ->
            :unknown

          {:side_load, paths} ->
            {:side_load, paths}

          {:dirty?, true} ->
            :unknown
        end

      %Not{expression: expression} ->
        case matches(record, expression, dirty_fields, side_loads) do
          :unknown ->
            :unknown

          {:ok, match?} ->
            {:ok, !match?}

          {:side_load, side_loads} ->
            {:side_load, side_loads}
        end

      %Expression{op: op, left: left, right: right} ->
        expression_matches(op, left, right, record, dirty_fields, side_loads)
    end
  end

  defp unique_calls([arg_values | rest]) do
    Enum.map(arg_values, fn value ->
      rest
      |> unique_calls()
      |> Enum.map(fn call ->
        [value | call]
      end)
    end)
  end

  defp unique_calls([]), do: []

  defp resolve_ref(%Ref{relationship_path: [], attribute: %{name: name}}, record) do
    [Map.get(record, name)]
  end

  defp resolve_ref(%Ref{attribute: %{name: name}, relationship_path: path}, record) do
    record
    |> get_related(path)
    |> Enum.map(&Map.get(&1, name))
  end

  defp resolve_ref(value, _record), do: value

  defp dirty?(fields, dirty) do
    dirty = dirty || []

    fields
    |> Enum.filter(&ref?/1)
    |> Enum.filter(&(&1.relationship_path == []))
    |> Enum.any?(&(&1.attribute.name in dirty))
  end

  defp need_to_load(fields, record) do
    fields
    |> Enum.filter(&ref?/1)
    |> Enum.filter(&(&1.relationship_path != []))
    |> Enum.reject(&loaded?(record, &1.relationship_path))
    |> Enum.map(& &1.relationship_path)
    |> Enum.map(fn path ->
      path_to_side_load(path)
    end)
  end

  defp path_to_side_load([first]), do: first

  defp path_to_side_load([first | rest]) do
    {first, [path_to_side_load(rest)]}
  end

  defp ref?(%Ash.Query.Ref{}), do: true
  defp ref?(_), do: false

  defp expression_matches(:and, left, right, record, dirty_fields, side_loads) do
    case matches(record, left, dirty_fields, side_loads) do
      {:ok, true} ->
        matches(record, right, dirty_fields, side_loads)

      :unknown ->
        :unknown

      {:ok, false} ->
        {:ok, false}

      {:side_load, side_loads} ->
        matches(record, right, dirty_fields, side_loads)
    end
  end

  defp expression_matches(:or, left, right, record, dirty_fields, side_loads) do
    case matches(record, left, dirty_fields, side_loads) do
      {:ok, true} ->
        {:ok, true}

      :unknown ->
        case matches(record, right, dirty_fields, side_loads) do
          {:ok, false} -> {:ok, :unknown}
          other -> other
        end

      {:ok, false} ->
        matches(record, right, dirty_fields, side_loads)

      {:side_load, side_loads} ->
        matches(record, right, dirty_fields, side_loads)
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
