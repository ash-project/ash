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

  alias Ash.Query.{BooleanExpression, Call, Not, Ref}

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
    filter = replace_calcs(filter)

    filter
    |> Ash.Filter.relationship_paths(true)
    |> Enum.reject(&(&1 == []))
    |> Enum.uniq()
    |> Enum.reject(&Ash.Resource.loaded?(records, &1))
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

  defp replace_calcs(filter) do
    Ash.Filter.map(filter, fn
      %Ash.Query.Ref{
        relationship_path: relationship_path,
        attribute: %Ash.Query.Calculation{module: module, opts: opts, context: context}
      } ->
        opts
        |> module.expression(context)
        |> replace_calcs()
        |> Ash.Filter.prefix_refs(relationship_path)

      other ->
        other
    end)
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
      |> Ash.Filter.relationship_paths(true)
      |> Enum.uniq()

    relationship_paths
    |> Enum.reject(&Ash.Resource.loaded?(record, &1))
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
            [record]

          [] ->
            [record]

          value when is_list(value) ->
            flatten_many_to_many(record, rel, value, rest)

          value ->
            flatten_to_one(record, rel, value, rest)
        end
      end)
    end)
  end

  defp flatten_many_to_many(record, rel, values, rest) do
    Enum.flat_map(values, fn value ->
      value
      |> flatten_relationships([rest])
      |> Enum.map(fn flattened_rest_value ->
        record
        |> Map.put(rel, flattened_rest_value)
        |> Ash.Resource.set_metadata(%{unflattened_rels: %{rel => values}})
      end)
    end)
  end

  defp flatten_to_one(record, rel, value, rest) do
    value
    |> flatten_relationships([rest])
    |> Enum.map(fn flattened_rest_value ->
      record
      |> Map.put(rel, flattened_rest_value)
      |> Ash.Resource.set_metadata(%{unflattened_rels: %{rel => value}})
    end)
  end

  @doc false
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

      %Ash.Query.Exists{} = expr ->
        resolve_expr(expr, record)

      %BooleanExpression{op: op, left: left, right: right} ->
        expression_matches(op, left, right, record)

      %Call{} = call ->
        raise "Unresolvable filter component: #{inspect(call)}"

      %Ref{} = ref ->
        resolve_expr(ref, record)

      other ->
        {:ok, other}
    end
  end

  defp load_unflattened(record, []), do: record
  defp load_unflattened(nil, _), do: nil

  defp load_unflattened(records, path) when is_list(records) do
    Enum.map(records, &load_unflattened(&1, path))
  end

  defp load_unflattened(record, [rel | rest]) do
    Map.put(
      record,
      rel,
      load_unflattened(record.__metadata__[:unflattened_rels][rel], rest)
    )
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
    resolve_ref(ref, record)
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

  defp resolve_expr(%Ash.Query.Exists{path: path, expr: expr}, record) do
    record
    |> load_unflattened(path)
    |> get_related(path)
    |> List.wrap()
    |> Enum.reduce_while({:ok, false}, fn related, {:ok, false} ->
      case resolve_expr(expr, related) do
        {:ok, falsy} when falsy in [nil, false] ->
          {:cont, {:ok, false}}

        {:ok, _} ->
          {:halt, {:ok, true}}

        other ->
          {:halt, other}
      end
    end)
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
        {:ok, nil}

      [] ->
        {:ok, nil}

      [record] ->
        {:ok, Map.get(record, name)}

      [%struct{} = record] ->
        if Spark.Dsl.is?(struct, Ash.Resource) do
          if Ash.Resource.Info.attribute(struct, name) do
            if Ash.Resource.selected?(record, name) do
              {:ok, Map.get(record, name)}
            else
              :unknown
            end
          else
            if Ash.Resource.loaded?(record, name) do
              {:ok, Map.get(record, name)}
            else
              :unknown
            end
          end
        else
          {:ok, Map.get(record, name)}
        end

      %struct{} = record ->
        if Spark.Dsl.is?(struct, Ash.Resource) do
          if Ash.Resource.Info.attribute(struct, name) do
            if Ash.Resource.selected?(record, name) do
              {:ok, Map.get(record, name)}
            else
              :unknown
            end
          else
            if Ash.Resource.loaded?(record, name) do
              {:ok, Map.get(record, name)}
            else
              :unknown
            end
          end
        else
          {:ok, Map.get(record, name)}
        end

      record ->
        {:ok, Map.get(record, name)}
    end
  end

  defp resolve_ref(value, _record), do: {:ok, value}

  defp path_to_load([first]), do: {first, []}

  defp path_to_load([first | rest]) do
    {first, [path_to_load(rest)]}
  end

  defp expression_matches(:and, left, right, record) do
    case do_match(record, left) do
      {:ok, false} ->
        {:ok, false}

      {:ok, nil} ->
        {:ok, false}

      {:ok, true} ->
        case do_match(record, right) do
          {:ok, false} ->
            {:ok, false}

          {:ok, nil} ->
            {:ok, false}

          {:ok, _} ->
            {:ok, true}

          :unknown ->
            :unknown
        end

      :unknown ->
        :unknown
    end
  end

  defp expression_matches(:or, left, right, record) do
    case do_match(record, left) do
      {:ok, falsy} when falsy in [nil, false] ->
        case do_match(record, right) do
          {:ok, falsy} when falsy in [nil, false] ->
            {:ok, false}

          {:ok, _} ->
            {:ok, true}

          :unknown ->
            :unknown
        end

      {:ok, _} ->
        {:ok, true}

      :unknown ->
        :unknown
    end
  end

  @doc false
  def get_related(record, []) do
    record
  end

  def get_related(records, paths) when is_list(records) do
    Enum.flat_map(records, fn record ->
      get_related(record, paths)
    end)
  end

  def get_related(record, [key | rest]) do
    case Map.get(record, key) do
      nil ->
        nil

      value ->
        get_related(value, rest)
    end
  end
end
