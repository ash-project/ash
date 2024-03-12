defmodule Ash.Actions.Read.Calculations do
  @moduledoc false
  def run([], _, _, _calculations_in_query), do: {:ok, []}

  def run(records, ash_query, calculations_at_runtime, calculations_in_query) do
    do_run_calculations(
      calculations_at_runtime,
      records,
      ash_query,
      MapSet.new(calculations_in_query, & &1.name)
    )
  end

  defp do_run_calculations(calculations, records, ash_query, done, tasks \\ [])
  defp do_run_calculations([], records, _ash_query, _done, []), do: {:ok, records}

  defp do_run_calculations(calculations, records, ash_query, done, tasks) do
    {do_now, do_later} =
      Enum.split_with(calculations, fn calc ->
        ash_query.context[:calculation_dependencies][calc.name]
        |> Kernel.||([])
        |> Enum.all?(&(&1 in done))
      end)

    if tasks == [] and do_now == [] do
      raise """
      Circular calculation dependency detected. Remaining calculations

      #{Enum.map_join(do_later, "\n\n", fn {key, calc} -> "* " <> inspect(calc) <> "\n" <> inspect(ash_query.context[:calculation_dependencies][key]) end)}
      """
    end

    {newly_done, remaining} =
      do_now
      |> Enum.map(fn calculation ->
        Ash.Actions.Read.AsyncLimiter.async_or_inline(
          ash_query,
          Ash.context_to_opts(calculation.context),
          fn ->
            {calculation.name, calculation, run_calculation(calculation, ash_query, records)}
          end
        )
      end)
      |> Enum.concat(tasks)
      |> Ash.Actions.Read.AsyncLimiter.await_at_least_one()

    records =
      Enum.reduce_while(newly_done, {:ok, records}, fn
        {_name, calculation, calculation_results}, {:ok, records} ->
          case calculation_results do
            {:ok, value} ->
              {:cont, {:ok, attach_calculation_results(calculation, records, value)}}

            {:error, error} ->
              {:halt, {:error, error}}

            nil ->
              {:cont, {:ok, records}}

            other ->
              {:cont, {:ok, attach_calculation_results(calculation, records, other)}}
          end
      end)

    case records do
      {:ok, records} ->
        do_run_calculations(
          do_later,
          records,
          ash_query,
          MapSet.union(MapSet.new(newly_done, &elem(&1, 0)), done),
          remaining
        )

      {:error, error} ->
        {:error, error}
    end
  end

  defp attach_calculation_results(calculation, records, nil) do
    if calculation.load do
      Enum.map(records, fn record ->
        Map.put(record, calculation.load, nil)
      end)
    else
      Enum.zip_with(records, fn record ->
        Map.update!(record, :calculations, &Map.put(&1, calculation.name, nil))
      end)
    end
  end

  defp attach_calculation_results(calculation, [%resource{} | _] = records, values)
       when is_map(values) and not is_struct(values) do
    case Ash.Resource.Info.primary_key(resource) do
      [key] ->
        if calculation.load do
          Enum.map(records, fn record ->
            value =
              case Map.fetch(values, Map.get(record, key)) do
                {:ok, value} ->
                  value

                :error ->
                  Map.get(record, Map.take(record, [key]))
              end

            Map.put(record, calculation.load, value)
          end)
        else
          Enum.map(records, fn record ->
            value =
              case Map.fetch(values, Map.get(record, key)) do
                {:ok, value} ->
                  value

                :error ->
                  Map.get(record, Map.take(record, [key]))
              end

            Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))
          end)
        end

      pkey ->
        if calculation.load do
          Enum.map(records, fn record ->
            value = Map.get(record, Map.take(record, pkey))

            Map.put(record, calculation.load, value)
          end)
        else
          Enum.map(records, fn record ->
            value = Map.get(record, Map.take(record, pkey))

            Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))
          end)
        end
    end
  end

  defp attach_calculation_results(calculation, records, values) do
    if calculation.load do
      Enum.zip_with([records, values], fn [record, value] ->
        Map.put(record, calculation.load, value)
      end)
    else
      Enum.zip_with([records, values], fn [record, value] ->
        Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))
      end)
    end
  end

  defp run_calculation(calculation, ash_query, records) do
    context = Map.put(calculation.context, :api, ash_query.api)

    records
    |> apply_transient_calculation_values(calculation, ash_query, [])
    |> calculation.module.calculate(calculation.opts, context)
    |> case do
      :unknown ->
        Enum.map(records, fn _ ->
          nil
        end)

      result ->
        result
    end
  end

  defp apply_transient_calculation_values(records, calculation, ash_query, path) do
    ash_query
    |> get_all_rewrites(calculation, path)
    |> rewrite(records)
  end

  def get_all_rewrites(ash_query, calculation, path) do
    Enum.flat_map(
      [:relationships, :calculations, :aggregates, :load_through],
      &get_rewrites(ash_query, calculation, path, &1)
    )
  end

  def rewrite([], records), do: records
  def rewrite(_rewrites, nil), do: nil

  def rewrite(rewrites, record) when not is_list(record) do
    rewrites
    |> rewrite([record])
    |> Enum.at(0)
  end

  def rewrite(rewrites, records) do
    rewrites
    |> Enum.sort_by(fn {{path, _, _, _}, _} ->
      Enum.count(path)
    end)
    |> Enum.reduce(records, fn
      {{path, data, calc_name, calc_load}, source}, records when path != [] ->
        rewrite_at_path(records, {{path, data, calc_name, calc_load}, source})

      {{_, {:calc, to_name, to_load}, _, _}, source}, records ->
        if to_load do
          Enum.map(records, fn record ->
            Map.put(record, to_load, Map.get(record.calculations, source))
          end)
        else
          Enum.map(records, fn record ->
            Map.update!(
              record,
              :calculations,
              &Map.put(&1, to_name, Map.get(record.calculations, source))
            )
          end)
        end

      {{_, {:agg, to_name, to_load}, _, _}, source}, records ->
        if to_load do
          Enum.map(records, fn record ->
            Map.put(record, to_load, Map.get(record.calculations, source))
          end)
        else
          Enum.map(records, fn record ->
            Map.update!(
              record,
              :aggregates,
              &Map.put(&1, to_name, Map.get(record.calculations, source))
            )
          end)
        end

      {{path, {:rel, rel_name}, _, _}, source}, records ->
        update_at_path(records, path, fn record ->
          Map.put(record, rel_name, Map.get(record.calculations, source))
        end)
    end)
  end

  # TODO: might consider grouping them by the first item in their path
  # so they can be applied at once? Even then, we iterate the whole thing multiple
  # times, so probably not an issue.
  defp rewrite_at_path(
         records,
         {{[{:attr, type, constraints, name} | rest], data, calc_name, calc_load}, source}
       ) do
    new_rewrites = [
      {{rest, data, calc_name, calc_load}, source}
    ]

    Enum.map(records, fn record ->
      Map.update!(record, name, &Ash.Type.rewrite(type, &1, new_rewrites, constraints))
    end)
  end

  defp rewrite_at_path(
         records,
         {{[{:calc, type, constraints, name, load} | rest], data, calc_name, calc_load}, source}
       ) do
    new_rewrites = [
      {{rest, data, calc_name, calc_load}, source}
    ]

    if load do
      Enum.map(records, fn record ->
        Map.update!(record, load, &Ash.Type.rewrite(type, &1, new_rewrites, constraints))
      end)
    else
      Enum.map(records, fn record ->
        Map.update!(record, :calculations, fn calculations ->
          calculations
          |> Map.put_new(name, nil)
          |> Map.update!(name, &Ash.Type.rewrite(type, &1, new_rewrites, constraints))
        end)
      end)
    end
  end

  defp update_at_path(nil, [], _), do: nil

  defp update_at_path(records, path, func) when is_list(records) do
    Enum.map(records, &update_at_path(&1, path, func))
  end

  defp update_at_path(record, [], func) do
    func.(record)
  end

  defp update_at_path(record, [key | rest], func) do
    Map.update!(record, key, &update_at_path(&1, rest, func))
  end

  defp load_through_calculation_rewrites(ash_query, calculation, path) do
    ash_query.load_through[:calculations]
    |> Kernel.||(%{})
    |> Enum.flat_map(fn {calc_name, further_load} ->
      through_calculation = ash_query.calculations[calc_name]

      if through_calculation && through_calculation.type do
        through_calculation.type
        |> Ash.Type.get_rewrites(further_load, calculation, path, calculation.constraints || [])
        |> Enum.map(fn
          {{path, rewrite_data, calc_name, calc_load}, source} ->
            {{path ++
                [
                  {:calc, through_calculation.type, through_calculation.constraints,
                   through_calculation.name, through_calculation.load}
                ], rewrite_data, calc_name, calc_load}, source}
        end)
      else
        []
      end
    end)
  end

  defp load_through_attribute_rewrites(ash_query, calculation, path) do
    ash_query.load_through[:attribute]
    |> Kernel.||(%{})
    |> Enum.flat_map(fn {attr_name, further_load} ->
      attr = Ash.Resource.Info.attribute(ash_query.resource, attr_name)

      attr.type
      |> Ash.Type.get_rewrites(further_load, calculation, path, attr.constraints)
      |> Enum.map(fn {{path, rewrite_data, calc_name, calc_load}, source} ->
        {{path ++ [{:attr, attr.type, attr.constraints, attr.name}], rewrite_data, calc_name,
          calc_load}, source}
      end)
    end)
  end

  defp get_rewrites(ash_query, calculation, path, :load_through) do
    load_through_calculation_rewrites(ash_query, calculation, path) ++
      load_through_attribute_rewrites(ash_query, calculation, path)
  end

  defp get_rewrites(ash_query, calculation, path, :relationships) do
    ash_query.load
    |> Enum.flat_map(fn
      {name, []} ->
        relationship = Ash.Resource.Info.relationship(ash_query.resource, name)

        relationship.destination
        |> Ash.Query.new()
        |> get_all_rewrites(calculation, path ++ [name])

      {name, query} ->
        get_all_rewrites(query, calculation, path ++ [name])
    end)
  end

  defp get_rewrites(ash_query, calculation, path, key) do
    calc_name = calculation.name
    calc_load = calculation.load

    ash_query
    |> Kernel.||(%{})
    |> Map.get(key, %{})
    |> Enum.flat_map(fn {_, calculation} ->
      case calculation.name do
        {:__calc_dep__, values} = source ->
          Enum.flat_map(values, fn
            {^path, {_, _, _}, ^calc_name, ^calc_load} = rewrite ->
              [{rewrite, source}]

            {^path, {:rel, name}, ^calc_name, ^calc_load} = rewrite ->
              [
                {rewrite, source}
                | get_all_rewrites(
                    calculation.opts[:query],
                    calculation,
                    path ++ [name]
                  )
              ]

            _ ->
              []
          end)

        _ ->
          []
      end
    end)
  end

  def split_and_load_calculations(api, ash_query, missing_pkeys?) do
    can_expression_calculation? =
      !missing_pkeys? &&
        Ash.DataLayer.data_layer_can?(ash_query.resource, :expression_calculation)

    ash_query =
      Enum.reduce(ash_query.calculations, ash_query, fn {_name, calc}, ash_query ->
        load_calculation_requirements(api, ash_query, calc, can_expression_calculation?)
      end)

    ash_query.calculations
    |> Map.values()
    |> Enum.reduce({[], [], ash_query}, fn calculation, {in_query, at_runtime, ash_query} ->
      if calculation.module.has_expression?() do
        expression =
          calculation.opts
          |> calculation.module.expression(calculation.context)
          |> Ash.Filter.build_filter_from_template(
            calculation.context[:actor],
            calculation.context,
            calculation.context
          )
          |> Ash.Actions.Read.add_calc_context_to_filter(
            calculation.context[:actor],
            calculation.context[:authorize?],
            calculation.context[:tenant],
            calculation.context[:tracer]
          )

        case Ash.Expr.eval(expression,
               resource: ash_query.resource,
               unknown_on_unknown_refs?: true
             ) do
          {:ok, result} ->
            {in_query,
             [
               %{
                 calculation
                 | module: Ash.Resource.Calculation.Literal,
                   opts: [value: result],
                   required_loads: [],
                   select: []
               }
               | at_runtime
             ], ash_query}

          _ ->
            if can_expression_calculation? do
              if all_referenced_calcs_support_expressions?(calculation, expression, ash_query) do
                {[calculation | in_query], at_runtime, ash_query}
              else
                {in_query, [calculation | at_runtime], ash_query}
              end
            else
              if calculation.module.has_calculate?() do
                {in_query, [calculation | at_runtime], ash_query}
              else
                {in_query,
                 [
                   %{
                     calculation
                     | module: Ash.Resource.Calculation.RuntimeExpression,
                       opts: [expr: expression],
                       required_loads: [],
                       select: []
                   }
                   | at_runtime
                 ], ash_query}
              end
            end
        end
      else
        {in_query, [calculation | at_runtime], ash_query}
      end
    end)
  end

  defp all_referenced_calcs_support_expressions?(calculation, expression \\ nil, ash_query) do
    expression =
      expression ||
        calculation.opts
        |> calculation.module.expression(calculation.context)
        |> Ash.Filter.build_filter_from_template(
          calculation.context[:actor],
          calculation.context,
          calculation.context
        )
        |> Ash.Actions.Read.add_calc_context_to_filter(
          calculation.context[:actor],
          calculation.context[:authorize?],
          calculation.context[:tenant],
          calculation.context[:tracer]
        )

    case Map.fetch(calculation.context, :all_referenced_calcs_support_expressions?) do
      {:ok, value} ->
        value

      :error ->
        expression
        |> Ash.Filter.hydrate_refs(%{resource: ash_query.resource, public?: false})
        |> case do
          {:ok, expression} ->
            expression
            |> Ash.Filter.used_calculations(ash_query.resource, :*)
            |> Enum.all?(fn %{module: module} ->
              module.has_expression?()
            end)

          :error ->
            true
        end
    end
  end

  defp load_calculation_requirements(
         api,
         query,
         calculation,
         can_expression_calculation?,
         calc_path \\ [],
         relationship_path \\ [],
         checked_calculations \\ []
       ) do
    if {calculation.module, calculation.opts} in checked_calculations do
      query
    else
      has_expression? = calculation.module.has_expression?()

      if has_expression? && all_referenced_calcs_support_expressions?(calculation, query) do
        Map.update!(query, :calculations, fn calculations ->
          Map.update!(calculations, calculation.name, fn calc ->
            Map.update!(calc, :context, fn context ->
              Map.put(context, :all_referenced_calcs_support_expressions?, true)
            end)
          end)
        end)
      else
        query =
          if has_expression? do
            Map.update!(query, :calculations, fn calculations ->
              Map.update!(calculations, calculation.name, fn calc ->
                Map.update!(calc, :context, fn context ->
                  Map.put(context, :all_referenced_calcs_support_expressions?, false)
                end)
              end)
            end)
          else
            query
          end

        calculation.required_loads
        |> List.wrap()
        |> Enum.concat(List.wrap(calculation.select))
        |> do_load_calculation_requirements(
          api,
          query,
          calculation.name,
          calculation.load,
          calc_path,
          relationship_path,
          can_expression_calculation?,
          [{calculation.module, calculation.opts} | checked_calculations]
        )
      end
    end
  end

  defp do_load_calculation_requirements(
         requirements,
         api,
         query,
         calc_name,
         calc_load,
         calc_path,
         relationship_path,
         can_expression_calculation?,
         checked_calculations
       ) do
    requirements
    |> Enum.map(fn
      {key, value} ->
        {key, value}

      key ->
        {key, []}
    end)
    |> Enum.reduce(query, fn
      {load, further}, query ->
        cond do
          match?(%Ash.Query.Calculation{}, load) ->
            load_depended_on_calc(
              query,
              load,
              further,
              api,
              calc_name,
              calc_load,
              calc_path,
              relationship_path,
              can_expression_calculation?,
              checked_calculations
            )

          match?(%Ash.Query.Aggregate{}, load) ->
            aggregate = load

            case find_equivalent_aggregate(query, aggregate) do
              {:ok, equivalent_aggregate} ->
                if equivalent_aggregate.load == aggregate.load and
                     equivalent_aggregate.name == aggregate.name do
                  query
                else
                  new_calc_name =
                    {:__calc_dep__,
                     [
                       {calc_path, {:agg, equivalent_aggregate.name, equivalent_aggregate.load},
                        calc_name, calc_load}
                     ]}

                  Ash.Query.calculate(
                    query,
                    new_calc_name,
                    {Ash.Resource.Calculation.FetchAgg,
                     load: equivalent_aggregate.name, name: equivalent_aggregate.load},
                    equivalent_aggregate.type,
                    equivalent_aggregate.context,
                    equivalent_aggregate.constraints
                  )
                  |> add_calculation_dependency(calc_name, new_calc_name)
                end

              :error ->
                new_agg =
                  if query.aggregates[aggregate.name] do
                    %{
                      aggregate
                      | name:
                          {:__calc_dep__,
                           [
                             {calc_path, {:agg, aggregate.name, aggregate.load}, calc_name,
                              calc_load}
                           ]},
                        load: nil
                    }
                  else
                    aggregate
                  end

                Ash.Query.load(query, new_agg)
            end

          attr = Ash.Resource.Info.attribute(query.resource, load) ->
            case Map.fetch(query.load_through[:attribute] || %{}, attr.name) do
              :error ->
                query =
                  Ash.Query.ensure_selected(query, attr.name)

                if further in [nil, []] do
                  query
                else
                  load_through =
                    query.load_through
                    |> Map.put_new(:attribute, %{})
                    |> Map.update!(:attribute, &Map.put(&1, attr.name, further))

                  %{query | load_through: load_through}
                end

              {:ok, value} ->
                load_through =
                  Map.update!(query.load_through, :attribute, fn attributes_load_through ->
                    Map.put(
                      attributes_load_through,
                      attr.name,
                      merge_load_through(
                        value || [],
                        further || [],
                        attr.type,
                        attr.constraints,
                        api,
                        calc_name,
                        calc_load,
                        calc_path,
                        relationship_path
                      )
                    )
                  end)

                %{
                  query
                  | load_through: load_through
                }
                |> Ash.Query.ensure_selected(attr.name)
            end

          agg = Ash.Resource.Info.aggregate(query.resource, load) ->
            Ash.Query.load(query, agg.name)

          resource_calculation = Ash.Resource.Info.calculation(query.resource, load) ->
            {args, load_through} =
              case further do
                {args, load_through} ->
                  {args, load_through}

                args ->
                  {args, nil}
              end

            {name, load} =
              cond do
                Keyword.keyword?(args) ->
                  case Keyword.fetch(args, :as) do
                    {:ok, value} ->
                      {value, nil}

                    :error ->
                      {resource_calculation.name, resource_calculation.name}
                  end

                is_map(args) ->
                  case Map.fetch(args, :as) do
                    {:ok, value} ->
                      {value, nil}

                    :error ->
                      {resource_calculation.name, resource_calculation.name}
                  end

                true ->
                  {resource_calculation.name, resource_calculation.name}
              end

            {module, opts} = resource_calculation.calculation

            with {:ok, args} <-
                   Ash.Query.validate_calculation_arguments(resource_calculation, args),
                 {:ok, calculation} <-
                   Ash.Query.Calculation.new(
                     name,
                     module,
                     opts,
                     {resource_calculation.type, resource_calculation.constraints},
                     Map.put(args, :context, query.context),
                     resource_calculation.filterable?,
                     resource_calculation.load
                   ) do
              calculation =
                Ash.Query.select_and_load_calc(
                  resource_calculation,
                  %{calculation | load: load, calc_name: resource_calculation.name},
                  query
                )

              load_depended_on_calc(
                query,
                calculation,
                load_through || [],
                api,
                calc_name,
                calc_load,
                calc_path,
                relationship_path,
                can_expression_calculation?,
                checked_calculations
              )
            else
              {:error, error} ->
                Ash.Query.add_error(query, :load, error)
            end

          relationship = Ash.Resource.Info.relationship(query.resource, load) ->
            query = Ash.Query.ensure_selected(query, relationship.source_attribute)

            case query.load[relationship.name] do
              nil ->
                Ash.Query.load(query, [{relationship.name, further}])

              current_load ->
                further = to_loaded_query(relationship.destination, further)

                if compatible_relationships?(current_load, further) do
                  %{
                    query
                    | load:
                        Keyword.put(
                          query.load,
                          relationship.name,
                          merge_query_load(
                            current_load,
                            further,
                            relationship.api || api,
                            calc_path,
                            calc_name,
                            calc_load,
                            relationship_path ++ [relationship.name]
                          )
                        )
                  }
                else
                  {type, constraints} =
                    case relationship.cardinality do
                      :many -> {{:array, :struct}, items: [instance_of: relationship.destination]}
                      :one -> {:struct, instance_of: relationship.destination}
                    end

                  case Enum.find(query.calculations, fn {_name, existing_calculation} ->
                         existing_calculation.module == Ash.Resource.Calculation.LoadRelationship &&
                           existing_calculation.opts[:relationship] == relationship.name &&
                           compatible_relationships?(existing_calculation.opts[:query], further) &&
                           match?(%{name: {:__calc_dep__, _}}, existing_calculation)
                       end) do
                    nil ->
                      new_calc_name =
                        {:__calc_dep__,
                         [
                           {calc_path, {:rel, relationship.name}, calc_name, calc_load}
                         ]}

                      query
                      |> Ash.Query.calculate(
                        new_calc_name,
                        {Ash.Resource.Calculation.LoadRelationship,
                         relationship: relationship.name,
                         query: further,
                         api: relationship.api || api},
                        type,
                        %{},
                        constraints
                      )
                      |> add_calculation_dependency(calc_name, new_calc_name)

                    {key, existing_calculation} ->
                      new_calculation =
                        existing_calculation
                        |> Map.update!(:opts, fn opts ->
                          Keyword.update(
                            opts,
                            :query,
                            further,
                            &merge_query_load(
                              &1,
                              further,
                              relationship.api || api,
                              calc_path,
                              calc_name,
                              calc_load,
                              relationship_path ++ [relationship.name]
                            )
                          )
                        end)
                        |> Map.update!(:name, fn {:__calc_dep__, paths} ->
                          {:__calc_dep__,
                           [{calc_path, {:rel, relationship.name}, calc_name, calc_load} | paths]}
                        end)

                      query
                      |> rename_and_replace_calculation(key, new_calculation)
                      |> add_calculation_dependency(calc_name, new_calculation.name)
                  end
                end
            end

          true ->
            raise "unknown load for #{inspect(query)}: #{inspect(load)}"
        end
    end)
  end

  defp rename_and_replace_calculation(query, current_key, new_calc) do
    new_calculations =
      query.calculations
      |> Map.delete(current_key)
      |> Map.put(new_calc.name, new_calc)
      |> Map.update!(:context, fn context ->
        Map.update(context, :calculation_dependencies, %{}, fn calculation_dependencies ->
          Map.new(calculation_dependencies, fn {key, value} ->
            new_key =
              if key == current_key do
                new_calc.name
              else
                key
              end

            {new_key,
             Enum.map(value, fn depends_on ->
               if depends_on == current_key do
                 new_calc.name
               else
                 depends_on
               end
             end)}
          end)
        end)
      end)

    new_load_through =
      if query.load_through[:calculation][current_key] do
        Map.update!(query.load_through, :calculation, fn load_through_calcs ->
          load_through_calcs
          |> Map.delete(current_key)
          |> Map.put(new_calc, load_through_calcs[current_key])
        end)
      else
        query.load_through
      end

    %{query | calculations: new_calculations, load_through: new_load_through}
  end

  defp load_depended_on_calc(
         query,
         calculation,
         further,
         api,
         calc_name,
         calc_load,
         calc_path,
         relationship_path,
         can_expression_calculation?,
         checked_calculations
       ) do
    case find_equivalent_calculation(query, calculation) do
      {:ok, equivalent_calculation} ->
        if equivalent_calculation.load == calculation.load and
             equivalent_calculation.name == calculation.name do
          add_calculation_dependency(query, calc_name, equivalent_calculation.name)
        else
          query =
            case Map.fetch(
                   query.load_through[:calculation] || %{},
                   equivalent_calculation.name
                 ) do
              :error ->
                if further in [nil, []] do
                  query
                else
                  load_through =
                    query.load_through
                    |> Map.put_new(:calculation, %{})
                    |> Map.update!(
                      :calculation,
                      &Map.put(&1, calculation.name, further)
                    )

                  %{query | load_through: load_through}
                end

              {:ok, value} ->
                load_through =
                  Map.update!(query.load_through, :calculation, fn calculations_load_through ->
                    Map.put(
                      calculations_load_through,
                      calculation.name,
                      merge_load_through(
                        value || [],
                        further || [],
                        calculation.type,
                        calculation.constraints,
                        api,
                        calc_name,
                        calc_load,
                        calc_path,
                        relationship_path
                      )
                    )
                  end)

                %{
                  query
                  | load_through: load_through
                }
            end

          new_calc_name =
            {:__calc_dep__,
             [
               {calc_path, {:calc, equivalent_calculation.name, equivalent_calculation.load},
                calc_name, calc_load}
             ]}

          Ash.Query.calculate(
            query,
            new_calc_name,
            {Ash.Resource.Calculation.FetchCalc,
             load: equivalent_calculation.name, name: equivalent_calculation.load},
            equivalent_calculation.type,
            equivalent_calculation.context,
            equivalent_calculation.constraints
          )
          |> add_calculation_dependency(calc_name, new_calc_name)
          |> add_calculation_dependency(new_calc_name, equivalent_calculation.name)
        end

      :error ->
        new_calculation =
          if query.calculations[calculation.name] do
            %{
              calculation
              | name:
                  {:__calc_dep__,
                   [
                     {calc_path, {:calc, calculation.name, calculation.load}, calc_name,
                      calc_load}
                   ]},
                load: nil
            }
          else
            calculation
          end

        query =
          Ash.Query.load(query, new_calculation)

        api
        |> load_calculation_requirements(
          query,
          new_calculation,
          can_expression_calculation?,
          relationship_path,
          relationship_path,
          checked_calculations
        )
        |> add_calculation_dependency(calc_name, new_calculation.name)
    end
  end

  defp add_calculation_dependency(query, source, dest) do
    %{
      query
      | context:
          query.context
          |> Map.put_new(:calculation_dependencies, %{})
          |> Map.update!(:calculation_dependencies, fn deps ->
            Map.update(deps, source, MapSet.new([dest]), &MapSet.put(&1, dest))
          end)
    }
  end

  defp find_equivalent_calculation(query, calculation) do
    Enum.find_value(query.calculations, :error, fn {_, other_calc} ->
      if other_calc.module == calculation.module and other_calc.opts == calculation.opts and
           other_calc.context == calculation.context do
        {:ok, other_calc}
      end
    end)
  end

  defp find_equivalent_aggregate(query, agg) do
    Enum.find_value(query.aggregates, :error, fn {_, other_agg} ->
      if other_agg == agg do
        {:ok, other_agg}
      end
    end)
  end

  defp compatible_relationships?(left, right) do
    # use this?
    # Ash.Query.equivalent_to?(left, right.filter) and
    left.resource == right.resource and
      left.filter == right.filter and
      left.sort == right.sort
  end

  defp merge_load_through(
         old,
         new,
         type,
         constraints,
         api,
         calc_name,
         calc_load,
         calc_path,
         relationship_path
       ) do
    case Ash.Type.merge_load(type, old, new, constraints, %{
           api: api,
           calc_name: calc_name,
           calc_load: calc_load,
           calc_path: calc_path,
           relationship_path: relationship_path
         }) do
      {:ok, result} ->
        result

      :error ->
        raise """
        Encountered a type #{inspect(type)} which cannot merge loads, and cannot be used in calculation dependencies.
        """

      {:error, error} ->
        raise """
        Encountered an error loading through type type #{inspect(type)}.

        Error: #{inspect(error)}
        """
    end
  end

  def merge_query_load(left, right, api, calc_path, calc_name, calc_load, relationship_path) do
    can_expression_calculation? =
      Ash.DataLayer.data_layer_can?(left.resource, :expression_calculation)

    do_load_calculation_requirements(
      right.load ++ Map.values(right.calculations) ++ Map.values(right.aggregates),
      api,
      left,
      calc_name,
      calc_load,
      calc_path,
      relationship_path,
      can_expression_calculation?,
      []
    )
  end

  defp to_loaded_query(resource, %Ash.Query{resource: resource} = query) do
    query
  end

  defp to_loaded_query(resource, loads) do
    Ash.Query.load(resource, loads)
  end

  # Deselect fields that we know statically cannot be seen
  # The field may be reselected later as a calculation dependency
  # this is an optimization not a guarantee
  def deselect_known_forbidden_fields(ash_query, calculations_at_runtime) do
    calculations_at_runtime
    |> Enum.reduce([], fn
      %{
        name: {:__ash_fields_are_visible__, fields},
        opts: opts
      },
      deselect_fields ->
        value =
          if Keyword.has_key?(opts, :expr) do
            # expression
            opts[:expr]
          else
            # literal
            opts[:value]
          end

        if value == false do
          deselect_fields ++ fields
        else
          deselect_fields
        end

      _, deselect_fields ->
        deselect_fields
    end)
    |> then(&unload_forbidden_fields(ash_query, &1))
  end

  defp unload_forbidden_fields(ash_query, fields) do
    fields
    |> Enum.group_by(fn field ->
      cond do
        Ash.Resource.Info.attribute(ash_query.resource, field) ->
          :attribute

        Ash.Resource.Info.aggregate(ash_query.resource, field) ->
          :aggregate

        Ash.Resource.Info.calculation(ash_query.resource, field) ->
          :calculation
      end
    end)
    |> Enum.reduce(ash_query, fn
      {:attribute, fields}, ash_query ->
        ash_query
        |> Ash.Query.deselect(fields)
        |> unload_attribute_calculations(fields)

      {:aggregate, fields}, ash_query ->
        unload_aggregates(ash_query, fields)

      {:calculation, fields}, ash_query ->
        unload_calculations(ash_query, fields)
    end)
  end

  defp unload_aggregates(ash_query, fields) do
    drop =
      ash_query.aggregates
      |> Enum.flat_map(fn {name, %{agg_name: agg_name}} ->
        if agg_name in fields do
          [name]
        else
          []
        end
      end)

    %{ash_query | aggregates: Map.drop(ash_query.aggregates, drop)}
  end

  defp unload_attribute_calculations(ash_query, fields) do
    drop =
      ash_query.calculations
      |> Enum.flat_map(fn
        {name, %{module: Ash.Resource.Calculation.LoadAttribute, opts: opts}} ->
          if opts[:attribute] in fields do
            [name]
          else
            []
          end

        _ ->
          []
      end)

    %{ash_query | calculations: Map.drop(ash_query.calculations, drop)}
  end

  defp unload_calculations(ash_query, fields) do
    drop =
      ash_query.calculations
      |> Enum.flat_map(fn
        {name, %{calc_name: calc_name}} ->
          if calc_name in fields do
            [name]
          else
            []
          end
      end)

    %{ash_query | calculations: Map.drop(ash_query.calculations, drop)}
  end
end
