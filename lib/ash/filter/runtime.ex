defmodule Ash.Filter.Runtime do
  @moduledoc """
  Checks a record to see if it matches a filter statement.

  We can't always tell if a record matches a filter statement, and as such this
  function may return `:unknown`. Additionally, some expressions wouldn't ever
  make sense outside of the context of the data layer, and will always be an
  error. For example, if you used the trigram search features in
  `ash_postgres`. That logic would need to be handwritten in Elixir and would
  need to be a *perfect* copy of the postgres implementation. That isn't a
  realistic goal. This generally should not affect anyone using the standard
  framework features, but if you were to attempt to use this module with a data
  layer like `ash_postgres`, certain expressions will behave unpredictably.
  """

  alias Ash.Query.{BooleanExpression, Not, Ref}

  @doc """
  Removes any records that don't match the filter. Automatically loads
  if necessary. If there are any ambigious terms in the filter (e.g things
  that could only be determined by data layer), it is assumed that they
  are not matches.
  """
  def filter_matches(api, records, filter, loaded? \\ false)
  def filter_matches(_api, [], _filter, _loaded), do: {:ok, []}

  def filter_matches(_api, records, nil, _loaded), do: {:ok, records}

  def filter_matches(api, records, filter, loaded?) do
    filter
    |> Ash.Filter.list_refs()
    |> Enum.map(& &1.relationship_path)
    |> Enum.reject(&(&1 == []))
    |> Enum.uniq()
    |> Enum.reject(&loaded?(records, &1))
    |> Enum.map(&path_to_load/1)
    |> case do
      [] ->
        {:ok,
         Enum.filter(records, fn record ->
           matches?(nil, record, filter)
         end)}

      need_to_load when not loaded? ->
        case api.load(records, need_to_load) do
          {:ok, loaded} ->
            filter_matches(api, loaded, filter, true)

          other ->
            other
        end

      _need_to_load when loaded? ->
        {:ok, []}
    end
  end

  @doc """
  Checks if a record matches a filter, loading any necessary relationships"

  If it can't tell, this returns false.
  """
  def matches?(api, record, filter) do
    case matches(record, filter) do
      {:ok, boolean} ->
        boolean

      {:load, loads} when not is_nil(api) ->
        matches?(api, api.load!(record, loads), filter)

      {:load, _} ->
        false
    end
  end

  def matches(record, expression) do
    relationship_paths =
      expression
      |> Ash.Filter.list_refs()
      |> Enum.map(& &1.relationship_path)
      |> Enum.uniq()

    relationship_paths
    |> Enum.reject(&loaded?(record, &1))
    |> case do
      [] ->
        {:ok,
         record
         |> flatten_relationships(relationship_paths)
         |> Enum.any?(fn scenario ->
           case do_match(scenario, expression) do
             {:ok, val} -> val
             _ -> false
           end
         end)}

      need_to_load ->
        {:load, Enum.map(need_to_load, &path_to_load/1)}
    end
  end

  defp flatten_relationships(record, relationship_paths) do
    relationship_paths
    |> Enum.reject(&(&1 == []))
    |> Enum.reduce([record], fn [rel | rest], records ->
      Enum.flat_map(records, fn record ->
        case Map.get(record, rel) do
          nil ->
            []

          [] ->
            []

          value when is_list(value) ->
            flatten_many_to_many(record, rel, value, rest)

          value ->
            flatten_to_one(record, rel, value, rest)
        end
      end)
    end)
  end

  defp flatten_many_to_many(record, rel, value, rest) do
    Enum.flat_map(value, fn value ->
      value
      |> flatten_relationships([rest])
      |> Enum.map(fn flattened_rest_value ->
        Map.put(record, rel, flattened_rest_value)
      end)
    end)
  end

  defp flatten_to_one(record, rel, value, rest) do
    value
    |> flatten_relationships([rest])
    |> Enum.map(fn flattened_rest_value ->
      Map.put(record, rel, flattened_rest_value)
    end)
  end

  def do_match(record, expression) do
    case expression do
      %Ash.Filter{expression: expression} ->
        do_match(record, expression)

      nil ->
        {:ok, true}

      %op{__operator__?: true, left: left, right: right} = operator ->
        with {:ok, [left, right]} <-
               resolve_exprs([left, right], record),
             {:known, val} <- op.evaluate(%{operator | left: left, right: right}) do
          {:ok, val}
        else
          {:error, error} ->
            {:error, error}

          :unknown ->
            :unknown

          _ ->
            {:ok, nil}
        end

      %func{__function__?: true, arguments: arguments} = function ->
        with {:ok, args} <- resolve_exprs(arguments, record),
             {:known, val} <- func.evaluate(%{function | arguments: args}) do
          {:ok, val}
        else
          {:error, error} ->
            {:error, error}

          :unknown ->
            :unknown

          _ ->
            {:ok, nil}
        end

      %Not{expression: expression} ->
        case do_match(record, expression) do
          :unknown ->
            :unknown

          {:ok, match?} ->
            {:ok, !match?}

          {:error, error} ->
            {:error, error}
        end

      %BooleanExpression{op: op, left: left, right: right} ->
        expression_matches(op, left, right, record)

      other ->
        {:ok, other}
    end
  end

  defp resolve_exprs(exprs, record) do
    exprs
    |> Enum.reduce_while({:ok, []}, fn expr, {:ok, exprs} ->
      case resolve_expr(expr, record) do
        {:ok, resolved} -> {:cont, {:ok, [resolved | exprs]}}
        {:error, error} -> {:halt, {:error, error}}
        :unknown -> {:halt, :unknown}
      end
    end)
    |> case do
      :unknown -> :unknown
      {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
      {:error, error} -> {:error, error}
    end
  end

  defp resolve_expr({key, value}, record) when is_atom(key) do
    case resolve_expr(value, record) do
      {:ok, resolved} ->
        {:ok, {key, resolved}}

      other ->
        other
    end
  end

  defp resolve_expr(%Ref{} = ref, record) do
    {:ok, resolve_ref(ref, record)}
  end

  defp resolve_expr(%BooleanExpression{left: left, right: right}, record) do
    with {:ok, left_resolved} <- resolve_expr(left, record),
         {:ok, right_resolved} <- resolve_expr(right, record) do
      {:ok, left_resolved && right_resolved}
    end
  end

  defp resolve_expr(%Not{expression: expression}, record) do
    case resolve_expr(expression, record) do
      {:ok, resolved} -> {:ok, !resolved}
      other -> other
    end
  end

  defp resolve_expr(%mod{__predicate__?: _, left: left, right: right} = pred, record) do
    with {:ok, [left, right]} <- resolve_exprs([left, right], record),
         {:known, val} <- mod.evaluate(%{pred | left: left, right: right}) do
      {:ok, val}
    else
      {:error, error} ->
        {:error, error}

      :unknown ->
        :unknown

      _ ->
        {:ok, nil}
    end
  end

  defp resolve_expr(%mod{__predicate__?: _, arguments: args} = pred, record) do
    with {:ok, args} <- resolve_exprs(args, record),
         {:known, val} <- mod.evaluate(%{pred | arguments: args}) do
      {:ok, val}
    else
      {:error, error} ->
        {:error, error}

      :unknown ->
        :unknown

      _ ->
        {:ok, nil}
    end
  end

  defp resolve_expr(other, _), do: {:ok, other}

  defp resolve_ref(%Ref{attribute: attribute, relationship_path: path}, record) do
    name =
      case attribute do
        %{name: name} -> name
        name -> name
      end

    record
    |> get_related(path)
    |> case do
      nil ->
        nil

      [] ->
        nil

      record ->
        Map.get(record, name)
    end
  end

  defp resolve_ref(value, _record), do: value

  defp path_to_load([first]), do: {first, []}

  defp path_to_load([first | rest]) do
    {first, [path_to_load(rest)]}
  end

  defp expression_matches(:and, left, right, record) do
    case do_match(record, left) do
      {:ok, true} ->
        do_match(record, right)

      {:ok, false} ->
        {:ok, false}

      {:ok, nil} ->
        {:ok, false}

      :unknown ->
        :unknown
    end
  end

  defp expression_matches(:or, left, right, record) do
    case do_match(record, left) do
      {:ok, true} ->
        {:ok, true}

      {:ok, false} ->
        do_match(record, right)

      {:ok, nil} ->
        do_match(record, right)

      :unknown ->
        :unknown
    end
  end

  defp get_related(record, []) do
    record
  end

  defp get_related(record, [key | rest]) do
    case Map.get(record, key) do
      nil ->
        nil

      value ->
        get_related(value, rest)
    end
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
