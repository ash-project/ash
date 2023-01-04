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
  if necessary. If there are any ambiguous terms in the filter (e.g things
  that could only be determined by data layer), it is assumed that they
  are not matches.
  """
  def filter_matches(api, records, filter, opts \\ [])
  def filter_matches(_api, [], _filter, _opts), do: {:ok, []}

  def filter_matches(_api, records, nil, _opts), do: {:ok, records}

  def filter_matches(api, records, filter, opts) do
    filter
    |> Ash.Filter.relationship_paths(true)
    |> Enum.reject(&(&1 == []))
    |> Enum.uniq()
    |> Enum.reject(&Ash.Resource.loaded?(records, &1))
    |> Enum.map(&path_to_load/1)
    |> case do
      [] ->
        Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
          matches = matches(record, filter, Keyword.put(opts, :this_loaded?, true))

          case matches do
            {:ok, falsey} when falsey in [false, nil] ->
              {:cont, {:ok, records}}

            {:ok, _} ->
              {:cont, {:ok, [record | records]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
        |> case do
          {:ok, records} ->
            {:ok, Enum.reverse(records)}

          other ->
            other
        end

      need_to_load ->
        case api.load(records, need_to_load) do
          {:ok, loaded} ->
            filter_matches(api, loaded, filter, opts)

          other ->
            other
        end
    end
  end

  def load_this_requirements(api, expression, this) do
    expression
    |> Ash.Filter.flat_map(fn %Ash.Query.This{expr: expr} ->
      expr
      |> Ash.Filter.relationship_paths(true)
      |> Enum.reject(&(&1 == []))
    end)
    |> Enum.uniq()
    |> Enum.reject(&Ash.Resource.loaded?(this, &1))
    |> case do
      [] ->
        {:ok, this}

      requirements ->
        api.load(this, requirements)
    end
  end

  defp matches(record, expression, opts) do
    relationship_paths =
      expression
      |> Ash.Filter.relationship_paths(true)
      |> Enum.uniq()

    relationship_paths
    |> Enum.reject(&Ash.Resource.loaded?(record, &1))
    |> case do
      [] ->
        record
        |> flatten_relationships(relationship_paths)
        |> Enum.reduce_while({:ok, false}, fn scenario, {:ok, false} ->
          case do_match(scenario, expression, opts[:this]) do
            {:error, error} ->
              {:halt, {:error, error}}

            :unknown ->
              {:halt, {:ok, false}}

            {:ok, val} when val ->
              {:halt, {:ok, true}}

            {:ok, _} ->
              {:cont, {:ok, false}}
          end
        end)

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
    full_flattened = Enum.map(values, &flatten_relationships(&1, [rest])) |> List.flatten()

    Enum.map(full_flattened, fn value ->
      record
      |> Map.put(rel, value)
      |> Ash.Resource.set_metadata(%{unflattened_rels: %{rel => full_flattened}})
    end)
  end

  defp flatten_to_one(record, rel, value, rest) do
    full_flattened = flatten_relationships(value, [rest])

    Enum.map(full_flattened, fn value ->
      record
      |> Map.put(rel, value)
      |> Ash.Resource.set_metadata(%{unflattened_rels: %{rel => full_flattened}})
    end)
  end

  @doc false
  def do_match(record, expression, this \\ nil) do
    hydrated =
      case record do
        %resource{} ->
          Ash.Filter.hydrate_refs(expression, %{
            resource: resource,
            public?: false
          })

        _ ->
          {:ok, expression}
      end

    case hydrated do
      {:ok, expression} ->
        case expression do
          %Ash.Filter{expression: expression} ->
            do_match(record, expression, this)

          %op{__operator__?: true, left: left, right: right} ->
            with {:ok, [left, right]} <-
                   resolve_exprs([left, right], record, this),
                 {:op, {:ok, %op{} = new_operator}} <-
                   {:op, Ash.Query.Operator.try_cast_with_ref(op, left, right)},
                 {:known, val} <-
                   op.evaluate(new_operator) do
              {:ok, val}
            else
              {:op, {:error, error}} ->
                {:error, error}

              {:op, {:ok, expr}} ->
                do_match(record, expr, this)

              {:error, error} ->
                {:error, error}

              :unknown ->
                :unknown

              _value ->
                :unknown
            end

          %func{__function__?: true, arguments: arguments} = function ->
            with {:ok, args} <- resolve_exprs(arguments, record, this),
                 {:args, args} when not is_nil(args) <-
                   {:args, try_cast_arguments(func.args(), args)},
                 {:known, val} <- func.evaluate(%{function | arguments: args}) do
              {:ok, val}
            else
              {:args, nil} ->
                {:error,
                 "Could not cast function arguments for #{func.name()}/#{Enum.count(arguments)}"}

              {:error, error} ->
                {:error, error}

              :unknown ->
                :unknown

              _ ->
                :unknown
            end

          %Not{expression: expression} ->
            case do_match(record, expression, this) do
              :unknown ->
                :unknown

              {:ok, match?} ->
                {:ok, !match?}

              {:error, error} ->
                {:error, error}
            end

          %Ash.Query.Exists{} = expr ->
            resolve_expr(expr, record, this)

          %Ash.Query.This{} = expr ->
            resolve_expr(expr, this, nil)

          %BooleanExpression{op: op, left: left, right: right} ->
            expression_matches(op, left, right, record, this)

          %Call{} = call ->
            raise "Unresolvable filter component: #{inspect(call)}"

          %Ref{} = ref ->
            resolve_expr(ref, record, this)

          value when is_list(value) ->
            value
            |> Enum.reduce_while({:ok, []}, fn value, {:ok, list} ->
              case do_match(record, value, this) do
                {:ok, result} ->
                  {:cont, {:ok, [result | list]}}

                other ->
                  {:halt, other}
              end
            end)
            |> case do
              {:ok, list} ->
                {:ok, Enum.reverse(list)}

              other ->
                other
            end

          other ->
            {:ok, other}
        end

      {:error, error} ->
        {:error, error}
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

  defp resolve_exprs(exprs, record, this) do
    exprs
    |> Enum.reduce_while({:ok, []}, fn expr, {:ok, exprs} ->
      case resolve_expr(expr, record, this) do
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

  defp resolve_expr({key, value}, record, this) when is_atom(key) do
    case resolve_expr(value, record, this) do
      {:ok, resolved} ->
        {:ok, {key, resolved}}

      other ->
        other
    end
  end

  defp resolve_expr(%Ref{} = ref, record, this) do
    resolve_ref(ref, record, this)
  end

  defp resolve_expr(%BooleanExpression{left: left, right: right}, record, this) do
    with {:ok, left_resolved} <- resolve_expr(left, record, this),
         {:ok, right_resolved} <- resolve_expr(right, record, this) do
      {:ok, left_resolved && right_resolved}
    end
  end

  defp resolve_expr(%Not{expression: expression}, record, this) do
    case resolve_expr(expression, record, this) do
      {:ok, resolved} -> {:ok, !resolved}
      other -> other
    end
  end

  defp resolve_expr(%Ash.Query.This{expr: expr}, _, this) do
    resolve_expr(expr, this, nil)
  end

  defp resolve_expr(%Ash.Query.Exists{}, nil, _this), do: :unknown

  defp resolve_expr(%Ash.Query.Exists{at_path: [], path: path, expr: expr}, record, _this) do
    record
    |> load_unflattened(path)
    |> get_related(path)
    |> case do
      :unknown ->
        :unknown

      related ->
        related
        |> List.wrap()
        |> Enum.reduce_while({:ok, false}, fn related, {:ok, false} ->
          case resolve_expr(expr, related, record) do
            {:ok, falsy} when falsy in [nil, false] ->
              {:cont, {:ok, false}}

            {:ok, _} ->
              {:halt, {:ok, true}}

            other ->
              {:halt, other}
          end
        end)
    end
  end

  defp resolve_expr(%Ash.Query.Exists{at_path: at_path} = exists, record, this) do
    record
    |> get_related(at_path)
    |> case do
      :unknown ->
        :unknown

      related ->
        related
        |> Enum.reduce_while({:ok, false}, fn related, {:ok, false} ->
          case resolve_expr(%{exists | at_path: []}, related, this) do
            {:ok, true} ->
              {:halt, {:ok, true}}

            {:ok, _} ->
              {:cont, {:ok, false}}

            other ->
              {:halt, other}
          end
        end)
    end
  end

  defp resolve_expr(%mod{__predicate__?: _, left: left, right: right}, record, this) do
    with {:ok, [left, right]} <- resolve_exprs([left, right], record, this),
         {:op, {:ok, %mod{} = new_pred}} <-
           {:op, Ash.Query.Operator.try_cast_with_ref(mod, left, right)},
         {:known, val} <- mod.evaluate(new_pred) do
      {:ok, val}
    else
      {:op, {:error, error}} ->
        {:error, error}

      {:op, {:ok, expr}} ->
        resolve_expr(expr, record, this)

      {:error, error} ->
        {:error, error}

      :unknown ->
        :unknown

      _ ->
        {:ok, nil}
    end
  end

  defp resolve_expr(%mod{__predicate__?: _, arguments: args} = pred, record, this) do
    with {:ok, args} <- resolve_exprs(args, record, this),
         {:args, args} when not is_nil(args) <-
           {:args, try_cast_arguments(mod.args(), args)},
         {:known, val} <- mod.evaluate(%{pred | arguments: args}) do
      {:ok, val}
    else
      {:args, nil} ->
        {:error, "Could not cast function arguments for #{mod.name()}/#{Enum.count(args)}"}

      {:error, error} ->
        {:error, error}

      :unknown ->
        :unknown

      _ ->
        {:ok, nil}
    end
  end

  defp resolve_expr(other, _, _), do: {:ok, other}

  defp try_cast_arguments(:var_args, args) do
    Enum.map(args, fn _ -> :any end)
  end

  defp try_cast_arguments(configured_args, args) do
    given_arg_count = Enum.count(args)

    configured_args
    |> Enum.filter(fn args ->
      Enum.count(args) == given_arg_count
    end)
    |> Enum.find_value(&Ash.Query.Function.try_cast_arguments(&1, args))
  end

  defp resolve_ref(_, nil, _), do: :unknown

  defp resolve_ref(
         %Ash.Query.Ref{
           relationship_path: relationship_path,
           attribute: %Ash.Query.Calculation{
             module: module,
             opts: opts,
             context: context,
             name: name
           }
         } = ref,
         record,
         this
       ) do
    if function_exported?(module, :expression, 2) do
      opts
      |> module.expression(context)
      |> Ash.Filter.prefix_refs(relationship_path)
      |> resolve_expr(record, this)
    else
      resolve_ref(%{ref | attribute: %Ash.Resource.Attribute{name: name}}, record, this)
    end
  end

  defp resolve_ref(%Ref{attribute: attribute, relationship_path: path}, record, _this) do
    name =
      case attribute do
        %{name: name} -> name
        name -> name
      end

    record
    |> get_related(path)
    |> case do
      :unknown ->
        :unknown

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

  defp resolve_ref(_value, _record, _), do: :unknown

  defp path_to_load([first]), do: {first, []}

  defp path_to_load([first | rest]) do
    {first, [path_to_load(rest)]}
  end

  defp expression_matches(:and, left, right, record, this) do
    case do_match(record, left, this) do
      {:ok, false} ->
        {:ok, false}

      {:ok, nil} ->
        {:ok, false}

      {:ok, true} ->
        case do_match(record, right, this) do
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

  defp expression_matches(:or, left, right, record, this) do
    case do_match(record, left, this) do
      {:ok, falsy} when falsy in [nil, false] ->
        case do_match(record, right, this) do
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
  def get_related(nil, _), do: []

  def get_related(%Ash.NotLoaded{}, []) do
    :unknown
  end

  def get_related(record, []) do
    record
  end

  def get_related(records, paths) when is_list(records) do
    records
    |> Enum.reduce_while([], fn
      :unknown, _records ->
        {:halt, :unknown}

      record, records ->
        case get_related(record, paths) do
          :unknown ->
            {:halt, :unknown}

          related ->
            {:cont, [related | records]}
        end
    end)
    |> case do
      :unknown ->
        :unknown

      records ->
        records
        |> List.flatten()
        |> Enum.reverse()
    end
  end

  def get_related(record, [key | rest]) do
    case Map.get(record, key) do
      nil ->
        []

      value ->
        case get_related(value, rest) do
          :unknown ->
            :unknown

          related ->
            List.wrap(related)
        end
    end
  end
end
