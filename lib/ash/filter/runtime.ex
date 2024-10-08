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
  if necessary. If there are any ambiguous terms in the filter (e.g things
  that could only be determined by data layer), it is assumed that they
  are not matches.
  """
  def filter_matches(domain, records, filter, opts \\ [])
  def filter_matches(_domain, [], _filter, _opts), do: {:ok, []}

  def filter_matches(_domain, records, nil, _opts), do: {:ok, records}

  def filter_matches(domain, records, filter, opts) do
    {records, parent} = load_records_and_parent(records, domain, opts[:parent], filter, opts)

    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      case matches(record, filter, Keyword.put(opts, :parent, parent)) do
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
  end

  defp load_parent(nil, _, _, _), do: nil
  defp load_parent([], _, _, _), do: []

  defp load_parent([parent | rest], domain, filter, opts) do
    filter =
      filter
      |> Ash.Filter.flat_map(fn
        %Ash.Query.Parent{expr: expr} ->
          [expr]

        _ ->
          []
      end)

    {parent, rest} =
      load_records_and_parent(
        parent,
        domain,
        rest,
        filter,
        Keyword.put(opts, :parent_loaded?, true)
      )

    [parent | rest]
  end

  defp load_parent(value, domain, filter, opts), do: load_parent([value], domain, filter, opts)

  defp load_records_and_parent(records, domain, parent, filter, opts) do
    resource =
      case records do
        %resource{} ->
          resource

        [%resource{} | _] ->
          resource
      end

    refs_to_load =
      filter
      |> Ash.Filter.list_refs(false, false, true)
      |> Enum.map(&(&1.relationship_path ++ [&1.attribute]))
      |> Enum.reject(&Ash.Resource.loaded?(records, &1))
      |> Enum.map(&path_to_load(resource, &1))

    records =
      case refs_to_load do
        [] ->
          records

        refs_to_load ->
          load =
            resource
            |> load_all(refs_to_load)
            |> Ash.Query.set_context(%{private: %{internal?: true}})

          Ash.load!(records, load,
            authorize?: false,
            domain: domain,
            tenant: opts[:tenant],
            actor: opts[:actor]
          )
      end

    parent =
      if opts[:parent_loaded?] do
        parent
      else
        load_parent(parent, domain, filter, opts)
      end

    {records, parent}
  end

  defp matches(record, expression, opts) do
    relationship_paths =
      expression
      |> Ash.Filter.relationship_paths(true)

    record
    |> flatten_relationships(relationship_paths)
    |> Enum.reduce_while({:ok, false}, fn scenario, {:ok, false} ->
      case do_match(
             scenario,
             expression,
             opts[:parent],
             nil,
             opts[:unknown_on_unknown_refs?],
             opts[:conflicting_upsert_values]
           ) do
        {:error, error} ->
          {:halt, {:error, error}}

        :unknown ->
          {:halt, {:ok, false}}

        {:ok, val} when val not in [false, nil] ->
          {:halt, {:ok, true}}

        {:ok, _} ->
          {:cont, {:ok, false}}
      end
    end)
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

          %Ash.NotLoaded{} = not_loaded ->
            [Ash.Resource.set_metadata(record, %{unflattened_rels: %{rel => not_loaded}})]

          %Ash.ForbiddenField{} = forbidden ->
            [Ash.Resource.set_metadata(record, %{unflattened_rels: %{rel => forbidden}})]

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
  def load_and_eval(
        record,
        expression,
        parent \\ nil,
        resource \\ nil,
        domain \\ nil,
        unknown_on_unknown_refs? \\ false,
        actor \\ nil,
        tenant \\ nil
      ) do
    if domain && record do
      refs_to_load =
        expression
        |> Ash.Filter.list_refs(false, false, true)
        |> Enum.map(&(&1.relationship_path ++ [&1.attribute]))
        |> Enum.reject(&Ash.Resource.loaded?(record, &1))
        |> Enum.map(&path_to_load(resource, &1))

      record =
        case refs_to_load do
          [] ->
            record

          refs ->
            load =
              resource
              |> load_all(refs)
              |> Ash.Query.set_context(%{private: %{internal?: true}})

            Ash.load!(record, load,
              domain: domain,
              authorize?: false,
              tenant: tenant,
              actor: actor
            )
        end

      do_match(record, expression, parent, resource, unknown_on_unknown_refs?)
    else
      do_match(record, expression, parent, resource, unknown_on_unknown_refs?)
    end
  end

  @doc false
  def do_match(
        record,
        expression,
        parent \\ nil,
        resource \\ nil,
        unknown_on_unknown_refs? \\ false,
        conflicting_upsert_values \\ nil
      )

  def do_match(
        record,
        %Ash.Filter.Simple{predicates: predicates},
        parent,
        resource,
        unknown_on_unknown_refs?,
        conflicting_upsert_values
      ) do
    {:ok,
     Enum.all?(predicates, fn predicate ->
       do_match(
         record,
         predicate,
         parent,
         resource,
         unknown_on_unknown_refs?,
         conflicting_upsert_values
       ) == {:ok, true}
     end)}
  end

  def do_match(
        record,
        expression,
        parent,
        resource,
        unknown_on_unknown_refs?,
        conflicting_upsert_values
      ) do
    hydrated =
      case record do
        %resource{} ->
          Ash.Filter.hydrate_refs(expression, %{
            resource: resource,
            public?: false,
            parent_stack: parent_stack(parent),
            conflicting_upsert_values: conflicting_upsert_values
          })

        _ ->
          if resource do
            Ash.Filter.hydrate_refs(expression, %{
              resource: resource,
              public?: false,
              parent_stack: parent_stack(parent),
              conflicting_upsert_values: conflicting_upsert_values
            })
          else
            {:ok, expression}
          end
      end

    with {:ok, hydrated} <- hydrated do
      case resolve_expr(hydrated, record, parent, resource, unknown_on_unknown_refs?) do
        :unknown ->
          if unknown_on_unknown_refs? do
            :unknown
          else
            {:ok, nil}
          end

        {:ok, value} ->
          {:ok, value}

        other ->
          other
      end
    end
  end

  defp load_all(query, paths) do
    Enum.reduce(paths, query, &Ash.Query.load(&2, &1))
  end

  defp load_unflattened(record, []), do: record
  defp load_unflattened(nil, _), do: nil

  defp load_unflattened(%Ash.NotLoaded{}, _), do: nil

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

  defp resolve_exprs(exprs, record, parent, resource, unknown_on_unknown_refs?) do
    exprs
    |> Enum.reduce_while({:ok, []}, fn expr, {:ok, exprs} ->
      case resolve_expr(expr, record, parent, resource, unknown_on_unknown_refs?) do
        {:ok, resolved} ->
          {:cont, {:ok, [resolved | exprs]}}

        {:error, error} ->
          {:halt, {:error, error}}

        :unknown ->
          {:halt, :unknown}
      end
    end)
    |> case do
      :unknown -> :unknown
      {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
      {:error, error} -> {:error, error}
    end
  end

  defp resolve_expr({:_actor, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_arg, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_ref, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_ref, _, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_parent, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_parent, _, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_atomic_ref, _}, _, _, _, _), do: :unknown
  defp resolve_expr({:_context, _}, _, _, _, _), do: :unknown

  defp resolve_expr(
         %Ash.Filter{expression: expression},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    resolve_expr(expression, record, parent, resource, unknown_on_unknown_refs?)
  end

  defp resolve_expr({key, value}, record, parent, resource, unknown_on_unknown_refs?)
       when is_atom(key) do
    case resolve_expr(value, record, parent, resource, unknown_on_unknown_refs?) do
      {:ok, resolved} ->
        {:ok, {key, resolved}}

      other ->
        other
    end
  end

  defp resolve_expr(%Ref{} = ref, record, parent, resource, unknown_on_unknown_refs?) do
    resolve_ref(ref, record, parent, resource, unknown_on_unknown_refs?)
  end

  defp resolve_expr(
         %BooleanExpression{op: :and, left: left, right: right},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    with {:ok, left_resolved} when left_resolved not in [nil, false] <-
           resolve_expr(left, record, parent, resource, unknown_on_unknown_refs?),
         {:ok, right_resolved} <-
           resolve_expr(right, record, parent, resource, unknown_on_unknown_refs?) do
      cond do
        is_nil(left_resolved) ->
          {:ok, nil}

        is_nil(right_resolved) ->
          {:ok, nil}

        true ->
          {:ok, !!left_resolved and !!right_resolved}
      end
    end
  end

  defp resolve_expr(
         %BooleanExpression{op: :or, left: left, right: right},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    with {:ok, left_resolved} when left_resolved in [nil, false] <-
           resolve_expr(left, record, parent, resource, unknown_on_unknown_refs?),
         {:ok, right_resolved} <-
           resolve_expr(right, record, parent, resource, unknown_on_unknown_refs?) do
      cond do
        left_resolved ->
          {:ok, !!left_resolved}

        is_nil(right_resolved) ->
          {:ok, right_resolved}

        true ->
          {:ok, !!right_resolved}
      end
    end
  end

  defp resolve_expr(
         %Not{expression: expression},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    case resolve_expr(expression, record, parent, resource, unknown_on_unknown_refs?) do
      {:ok, nil} -> {:ok, nil}
      {:ok, resolved} -> {:ok, !resolved}
      other -> other
    end
  end

  defp resolve_expr(
         %Ash.CustomExpression{simple_expression: {:ok, expression}},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    case resolve_expr(expression, record, parent, resource, unknown_on_unknown_refs?) do
      {:ok, resolved} -> {:ok, resolved}
      other -> other
    end
  end

  defp resolve_expr(
         %Ash.CustomExpression{expression: expression},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    case resolve_expr(expression, record, parent, resource, unknown_on_unknown_refs?) do
      {:ok, resolved} -> {:ok, resolved}
      other -> other
    end
  end

  defp resolve_expr(
         %Ash.Query.Parent{},
         _,
         parent,
         _resource,
         unknown_on_unknown_refs?
       )
       when is_nil(parent) or parent == [] do
    if unknown_on_unknown_refs? do
      :unknown
    else
      {:ok, nil}
    end
  end

  defp resolve_expr(
         %Ash.Query.Parent{} = parent_expr,
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       )
       when not is_list(parent) do
    resolve_expr(parent_expr, record, [parent], resource, unknown_on_unknown_refs?)
  end

  defp resolve_expr(
         %Ash.Query.Parent{expr: expr},
         _,
         [parent | rest],
         resource,
         unknown_on_unknown_refs?
       ) do
    resolve_expr(expr, parent, rest, resource, unknown_on_unknown_refs?)
  end

  defp resolve_expr(
         %Ash.Query.UpsertConflict{} = expr,
         _record,
         _parent,
         _resource,
         _unknown_on_unknown_refs?
       ) do
    {:error, "#{inspect(expr)} not implemented for data source"}
  end

  defp resolve_expr(%Ash.Query.Exists{}, nil, _parent, _resource, unknown_on_unknown_refs?) do
    if unknown_on_unknown_refs? do
      :unknown
    else
      {:ok, nil}
    end
  end

  defp resolve_expr(
         %Ash.Query.Exists{at_path: [], path: path, expr: expr},
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    record
    |> flatten_relationships([path])
    |> load_unflattened(path)
    |> get_related(path, unknown_on_unknown_refs?)
    |> case do
      :unknown ->
        :unknown

      related ->
        related
        |> List.wrap()
        |> Enum.reduce_while({:ok, false}, fn related, {:ok, false} ->
          case resolve_expr(
                 expr,
                 related,
                 [record | List.wrap(parent)],
                 resource,
                 unknown_on_unknown_refs?
               ) do
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

  defp resolve_expr(
         %Ash.Query.Exists{at_path: at_path} = exists,
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    record
    |> flatten_relationships([at_path])
    |> get_related(at_path, unknown_on_unknown_refs?)
    |> case do
      :unknown ->
        :unknown

      related ->
        related
        |> Enum.reduce_while({:ok, false}, fn related, {:ok, false} ->
          case resolve_expr(
                 %{exists | at_path: []},
                 related,
                 parent,
                 resource,
                 unknown_on_unknown_refs?
               ) do
            {:ok, true} ->
              {:halt, {:ok, true}}

            {:ok, _} ->
              {:cont, {:ok, false}}

            :unknown ->
              {:halt, :unknown}

            other ->
              {:halt, other}
          end
        end)
    end
  end

  defp resolve_expr(
         %mod{__predicate__?: _, left: left, right: right} = pred,
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    case partial_evaluate(
           pred,
           record,
           parent,
           resource,
           unknown_on_unknown_refs?
         ) do
      {:ok, ^pred} ->
        with {:ok, [left, right]} <-
               resolve_exprs([left, right], record, parent, resource, unknown_on_unknown_refs?),
             {:op, {:ok, new_pred}} <-
               {:op, Ash.Query.Operator.try_cast_with_ref(mod, left, right)},
             {:known, val} <-
               evaluate(new_pred, record, parent, resource, unknown_on_unknown_refs?) do
          {:ok, val}
        else
          {:op, {:error, error}} ->
            {:error, error}

          {:op, {:ok, expr}} ->
            resolve_expr(expr, record, parent, resource, unknown_on_unknown_refs?)

          {:error, error} ->
            {:error, error}

          {:op, :unknown} ->
            :unknown

          :unknown ->
            :unknown

          _ ->
            {:ok, nil}
        end

      {:ok, other} ->
        resolve_expr(other, record, parent, resource, unknown_on_unknown_refs?)

      {:error, error} ->
        {:error, error}
    end
  end

  defp resolve_expr(
         %mod{__predicate__?: _, arguments: args} = pred,
         record,
         parent,
         resource,
         unknown_on_unknown_refs?
       ) do
    case partial_evaluate(
           pred,
           record,
           parent,
           resource,
           unknown_on_unknown_refs?
         ) do
      {:ok, ^pred} ->
        # resolve arguments one at a time from left to right, and attempt to partial evaluate again.
        # required for short circuiting `and`, `or`, `||`, `&&` and `if`. Its kind of a hacky
        # short circuiting but no functions that I know of need different behavior,
        # and if they do we can add a new callback or something.
        case Enum.find_index(args, &Ash.Expr.expr?/1) do
          index when is_integer(index) ->
            case resolve_expr(
                   Enum.at(args, index),
                   record,
                   parent,
                   resource,
                   unknown_on_unknown_refs?
                 ) do
              {:ok, new_value} ->
                resolve_expr(
                  %{
                    pred
                    | arguments: List.replace_at(args, index, new_value)
                  },
                  record,
                  parent,
                  resource,
                  unknown_on_unknown_refs?
                )

              {:error, error} ->
                {:error, error}

              :unknown ->
                :unknown
            end

          nil ->
            with {:ok, args} <-
                   resolve_exprs(args, record, parent, resource, unknown_on_unknown_refs?),
                 {:args, args} when not is_nil(args) <-
                   {:args, try_cast_arguments(mod.args(), args)},
                 {:known, val} <-
                   evaluate(
                     %{pred | arguments: args},
                     record,
                     parent,
                     resource,
                     unknown_on_unknown_refs?
                   ) do
              {:ok, val}
            else
              {:args, nil} ->
                {:error,
                 "Could not cast function arguments for #{mod.name()}/#{Enum.count(args)}"}

              {:error, error} ->
                {:error, error}

              :unknown ->
                :unknown

              _ ->
                {:ok, nil}
            end
        end

      {:ok, other} ->
        resolve_expr(other, record, parent, resource, unknown_on_unknown_refs?)

      {:error, error} ->
        {:error, error}
    end
  end

  defp resolve_expr(list, record, parent, resource, unknown_on_unknown_refs?)
       when is_list(list) do
    list
    |> Enum.reduce_while({:ok, []}, fn item, {:ok, acc} ->
      case resolve_expr(item, record, parent, resource, unknown_on_unknown_refs?) do
        {:ok, result} ->
          {:cont, {:ok, [result | acc]}}

        :unknown ->
          {:halt, :unknown}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, results} -> {:ok, Enum.reverse(results)}
      other -> other
    end
  end

  defp resolve_expr(map, record, parent, resource, unknown_on_unknown_refs?)
       when is_map(map) and not is_struct(map) do
    Enum.reduce_while(map, {:ok, %{}}, fn {key, value}, {:ok, acc} ->
      with {:ok, key} <- resolve_expr(key, record, parent, resource, unknown_on_unknown_refs?),
           {:ok, value} <- resolve_expr(value, record, parent, resource, unknown_on_unknown_refs?) do
        {:cont, {:ok, Map.put(acc, key, value)}}
      else
        other ->
          {:halt, other}
      end
    end)
  end

  defp resolve_expr(other, _, _, _, _), do: {:ok, other}

  defp try_cast_arguments(:var_args, args) do
    args
    |> Enum.map(fn _ -> :any end)
    |> Ash.Query.Function.try_cast_arguments(args)
  end

  defp try_cast_arguments(configured_args, args) do
    given_arg_count = Enum.count(args)

    configured_args
    |> Enum.filter(fn args ->
      Enum.count(args) == given_arg_count
    end)
    |> Enum.find_value(&Ash.Query.Function.try_cast_arguments(&1, args))
  end

  defp resolve_ref(
         %Ash.Query.Ref{
           relationship_path: relationship_path,
           resource: resource,
           attribute: %Ash.Query.Calculation{
             module: module,
             opts: opts,
             name: name,
             load: load,
             context: context
           }
         },
         record,
         parent,
         _resource,
         unknown_on_unknown_refs?
       ) do
    result =
      record
      |> get_related(relationship_path, unknown_on_unknown_refs?)
      |> case do
        :unknown ->
          if unknown_on_unknown_refs? do
            :unknown
          else
            {:ok, nil}
          end

        [] ->
          {:ok, nil}

        [record | _] ->
          if load do
            case Map.fetch(record || %{}, load) do
              :error ->
                :unknown

              {:ok, %Ash.NotLoaded{}} ->
                :unknown

              {:ok, %Ash.ForbiddenField{}} ->
                :unknown

              {:ok, other} ->
                {:ok, other}
            end
          else
            case Map.fetch(Map.get(record || %{}, :calculations, %{}), name) do
              {:ok, value} ->
                {:ok, value}

              :error ->
                :unknown
            end
          end
      end

    case result do
      {:ok, result} ->
        {:ok, result}

      :unknown ->
        if module.has_expression?() do
          expression = module.expression(opts, context)

          hydrated =
            Ash.Filter.hydrate_refs(expression, %{
              resource: resource,
              public?: false,
              parent_stack: parent_stack(parent)
            })

          with {:ok, hydrated} <- hydrated do
            hydrated
            |> Ash.Filter.prefix_refs(relationship_path)
            |> resolve_expr(record, parent, resource, unknown_on_unknown_refs?)
          end
        else
          # We need to rewrite this
          # As it stands now, it will evaluate the calculation
          # once per expanded result. I'm not sure what that will
          # look like though.

          if record do
            case module.calculate([record], opts, context) do
              [result] ->
                {:ok, result}

              {:ok, [result]} ->
                {:ok, result}

              :unknown when unknown_on_unknown_refs? ->
                :unknown

              _ ->
                {:ok, nil}
            end
          else
            if unknown_on_unknown_refs? do
              :unknown
            else
              {:ok, nil}
            end
          end
        end
    end
  end

  defp resolve_ref(_ref, nil, _, _resource, unknown_on_unknown_refs?) do
    if unknown_on_unknown_refs? do
      :unknown
    else
      {:ok, nil}
    end
  end

  defp resolve_ref(
         %Ash.Query.Ref{
           relationship_path: [],
           attribute: %Ash.Query.Aggregate{
             load: load,
             name: name
           }
         },
         record,
         _parent,
         _resource,
         unknown_on_unknown_refs?
       ) do
    if load do
      case Map.get(record, load) do
        %Ash.ForbiddenField{} ->
          if unknown_on_unknown_refs? do
            :unknown
          else
            {:ok, nil}
          end

        %Ash.NotLoaded{} ->
          if unknown_on_unknown_refs? do
            :unknown
          else
            {:ok, nil}
          end

        other ->
          {:ok, other}
      end
    else
      case Map.fetch(record.aggregates, name) do
        {:ok, value} ->
          {:ok, value}

        :error ->
          if unknown_on_unknown_refs? do
            :unknown
          else
            {:ok, nil}
          end
      end
    end
  end

  defp resolve_ref(
         %Ref{attribute: attribute, relationship_path: path},
         record,
         _parent,
         _resource,
         unknown_on_unknown_refs?
       ) do
    name =
      case attribute do
        %{name: name} -> name
        name -> name
      end

    record
    |> get_related(path, unknown_on_unknown_refs?)
    |> case do
      :unknown ->
        if unknown_on_unknown_refs? do
          :unknown
        else
          {:ok, nil}
        end

      [] ->
        {:ok, nil}

      [%struct{} = record | _] ->
        if Spark.Dsl.is?(struct, Ash.Resource) do
          if Ash.Resource.Info.attribute(struct, name) do
            if Ash.Resource.selected?(record, name) do
              {:ok, Map.get(record, name)}
            else
              if unknown_on_unknown_refs? do
                :unknown
              else
                {:ok, nil}
              end
            end
          else
            if Ash.Resource.loaded?(record, name) do
              {:ok, Map.get(record, name)}
            else
              if unknown_on_unknown_refs? do
                :unknown
              else
                {:ok, nil}
              end
            end
          end
        else
          {:ok, Map.get(record, name)}
        end

      [record | _] ->
        {:ok, Map.get(record, name)}

      _ ->
        {:ok, nil}
    end
    |> or_default(attribute)
  end

  defp resolve_ref(_value, _record, _, _, true), do: :unknown
  defp resolve_ref(_value, _record, _, _, _), do: {:ok, nil}

  defp or_default({:ok, nil}, %Ash.Resource.Aggregate{default: default})
       when not is_nil(default) do
    if is_function(default) do
      {:ok, default.()}
    else
      {:ok, default}
    end
  end

  defp or_default({:ok, nil}, %Ash.Query.Aggregate{default_value: default})
       when not is_nil(default) do
    if is_function(default) do
      {:ok, default.()}
    else
      {:ok, default}
    end
  end

  defp or_default(other, _), do: other

  defp path_to_load(_resource, [%struct{name: name}])
       when struct in [
              Ash.Resource.Attribute,
              Ash.Resource.Aggregate,
              Ash.Resource.Calculation
            ] do
    [name]
  end

  defp path_to_load(_, []), do: []

  defp path_to_load(_resource, [other]) do
    [other]
  end

  defp path_to_load(resource, [first | rest]) do
    [{first, path_to_load(resource, rest)}]
  end

  @doc false
  def get_related(
        source,
        path,
        unknown_on_unknown_refs?,
        join_filters \\ %{},
        parent_stack \\ [],
        domain \\ nil
      )

  def get_related(nil, _, unknown_on_unknown_refs?, _join_filters, _parent_stack, _domain) do
    if unknown_on_unknown_refs? do
      :unknown
    else
      []
    end
  end

  def get_related(
        %Ash.ForbiddenField{},
        _,
        unknown_on_unknown_refs?,
        _join_filters,
        _parent_stack,
        _domain
      ) do
    if unknown_on_unknown_refs? do
      :unknown
    else
      []
    end
  end

  def get_related(
        %Ash.NotLoaded{},
        _,
        unknown_on_unknown_refs?,
        _join_filters,
        _parent_stack,
        _domain
      ) do
    if unknown_on_unknown_refs? do
      :unknown
    else
      []
    end
  end

  def get_related(record, [], _, _, _parent_stack, _domain) do
    List.wrap(record)
  end

  def get_related(
        records,
        [key | rest],
        unknown_on_unknown_refs?,
        join_filters,
        parent_stack,
        domain
      )
      when is_list(records) do
    {join_filter, rest_join_filters} = Map.pop(join_filters, [])

    rest_join_filters =
      Enum.reduce(rest_join_filters, %{}, fn {path, filter}, acc ->
        if List.starts_with?(path, [key]) do
          Map.put(acc, Enum.drop(path, 1), filter)
        else
          acc
        end
      end)

    filtered =
      if Map.has_key?(join_filters, []) do
        filter_matches(domain, records, join_filter,
          parent: parent_stack,
          unknown_on_unknown_refs?: unknown_on_unknown_refs?
        )
      else
        {:ok, records}
      end

    case filtered do
      {:ok, matches} ->
        matches
        |> Enum.reduce_while([], fn match, acc ->
          case Map.get(match, key) do
            %Ash.NotLoaded{} when unknown_on_unknown_refs? ->
              {:halt, :unknown}

            %Ash.ForbiddenField{} when unknown_on_unknown_refs? ->
              {:halt, :unknown}

            nil when unknown_on_unknown_refs? ->
              {:halt, :unknown}

            nil ->
              {:cont, acc}

            match_keys ->
              match_keys
              |> List.wrap()
              |> Enum.reduce_while([], fn this_match, inner_acc ->
                case get_related(
                       this_match,
                       rest,
                       unknown_on_unknown_refs?,
                       rest_join_filters,
                       [match | parent_stack],
                       domain
                     ) do
                  :unknown -> {:halt, :unknown}
                  value -> {:cont, inner_acc ++ List.wrap(value)}
                end
              end)
              |> case do
                :unknown -> {:halt, :unknown}
                list -> {:cont, acc ++ list}
              end
          end
        end)

      _ ->
        if unknown_on_unknown_refs? do
          :unknown
        else
          []
        end
    end
  end

  def get_related(
        record,
        [key | _] = path,
        unknown_on_unknown_refs?,
        join_filters,
        parent_stack,
        domain
      ) do
    case Map.get(record, key) do
      %Ash.NotLoaded{} when unknown_on_unknown_refs? ->
        :unknown

      %Ash.ForbiddenField{} when unknown_on_unknown_refs? ->
        :unknown

      nil when unknown_on_unknown_refs? ->
        :unknown

      _value ->
        case get_related(
               [record],
               path,
               unknown_on_unknown_refs?,
               join_filters,
               parent_stack,
               domain
             ) do
          :unknown ->
            if unknown_on_unknown_refs? do
              :unknown
            else
              []
            end

          related ->
            List.wrap(related)
        end
    end
  end

  def old_get_related(
        records,
        [key | rest],
        unknown_on_unknown_refs?,
        join_filters,
        parent_stack,
        domain
      )
      when is_list(records) do
    {join_filter, rest_join_filters} = Map.pop(join_filters, [])

    rest_join_filters =
      Enum.reduce(rest_join_filters, %{}, fn {path, filter}, acc ->
        if List.starts_with?(path, [key]) do
          Map.put(acc, Enum.drop(path, 1), filter)
        else
          acc
        end
      end)

    filtered =
      if Map.has_key?(join_filters, []) do
        filter_matches(domain, records, join_filter,
          parent: parent_stack,
          unknown_on_unknown_refs?: unknown_on_unknown_refs?
        )
      else
        {:ok, records}
      end

    case filtered do
      {:ok, matches} ->
        matches
        |> Enum.flat_map(fn match ->
          case Map.get(match, key) do
            :unknown ->
              []

            nil ->
              []

            matches ->
              matches
              |> List.wrap()
              |> Enum.flat_map(&List.wrap/1)
              |> Enum.reject(&(&1 in [:unknown, nil]))
              |> Enum.flat_map(fn this_match ->
                case get_related(
                       this_match,
                       rest,
                       unknown_on_unknown_refs?,
                       rest_join_filters,
                       [match | parent_stack],
                       domain
                     ) do
                  :unknown -> []
                  value -> value
                end
              end)
              |> List.flatten()
          end
        end)

      _ ->
        if unknown_on_unknown_refs? do
          :unknown
        else
          []
        end
    end
  end

  defp parent_stack(parent) do
    parent
    |> List.wrap()
    |> Enum.map(fn
      %resource{} ->
        resource

      resource ->
        resource
    end)
  end

  defp evaluate(
         %{__function__?: true} = func,
         _record,
         _parent,
         _resource,
         _unknown_on_unknown_refs?
       ),
       do: Ash.Query.Function.evaluate(func)

  defp evaluate(
         %{__operator__?: true} = op,
         _record,
         _parent,
         _resource,
         _unknown_on_unknown_refs?
       ),
       do: Ash.Query.Operator.evaluate(op)

  defp evaluate(other, record, parent, resource, unknown_on_unknown_refs?) do
    case resolve_expr(other, record, parent, resource, unknown_on_unknown_refs?) do
      {:ok, value} -> {:known, value}
      other -> other
    end
  end

  defp partial_evaluate(
         %mod{__predicate__?: _} = pred,
         _record,
         _parent,
         _resource,
         _unknown_on_unknown_refs?
       ) do
    if function_exported?(mod, :partial_evaluate, 1) do
      mod.partial_evaluate(pred)
    else
      {:ok, pred}
    end
  end

  defp partial_evaluate(other, _record, _parent, _resource, _unknown_on_unknown_refs?) do
    {:ok, other}
  end
end
