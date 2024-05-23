defmodule Ash.Actions.Read.Calculations do
  @moduledoc false

  def calculate(resource_or_record, calculation, opts) do
    {resource, record} =
      case resource_or_record do
        %resource{} = record -> {resource, record}
        resource -> {resource, opts[:record]}
      end

    with {:calc,
          %{
            arguments: calc_arguments,
            calculation: {module, calc_opts},
            type: type,
            constraints: constraints
          }} <- {:calc, Ash.Resource.Info.calculation(resource, calculation)},
         record <- struct(record || resource, opts[:refs] || %{}) do
      args = opts[:args] || %{}

      arguments =
        Enum.reduce(calc_arguments, %{}, fn arg, arguments ->
          if Map.has_key?(args, arg.name) do
            Map.put(arguments, arg.name, args[arg.name])
          else
            if is_nil(arg.default) do
              arguments
            else
              Map.put(arguments, arg.name, arg.default)
            end
          end
        end)

      primary_key =
        case Ash.Resource.Info.primary_key(resource) do
          [] ->
            nil

          primary_key ->
            Enum.reduce_while(primary_key, %{}, fn key, acc ->
              case Map.get(record, key) do
                nil -> {:halt, nil}
                %Ash.NotLoaded{} -> {:halt, nil}
                other -> {:cont, Map.put(acc, key, other)}
              end
            end)
        end

      calc_context =
        %Ash.Resource.Calculation.Context{
          actor: opts[:actor],
          domain: opts[:domain],
          tenant: opts[:tenant],
          authorize?: opts[:authorize?],
          tracer: opts[:tracer],
          resource: opts[:resource],
          arguments: arguments,
          type: type,
          constraints: constraints,
          source_context: opts[:context] || %{}
        }

      if module.has_expression?() do
        expr =
          case module.expression(calc_opts, calc_context) do
            {:ok, result} -> {:ok, result}
            {:error, error} -> {:error, error}
            result -> {:ok, result}
          end

        with {:ok, expr} <- expr,
             {:ok, expr} <-
               Ash.Filter.hydrate_refs(expr, %{resource: resource}) do
          if is_nil(primary_key) && requires_primary_key?(expr) do
            {:error,
             Ash.Error.Query.CalculationRequiresPrimaryKey.exception(
               resource: resource,
               calculation: calculation
             )}
          else
            if is_nil(primary_key) do
              expr = replace_refs(expr, Keyword.put(opts, :record, record))

              evaled =
                try do
                  Ash.Expr.eval(expr,
                    record: record,
                    resource: resource,
                    unknown_on_unknown_refs?: true
                  )
                rescue
                  _ ->
                    :unknown
                end

              case evaled do
                {:ok, result} ->
                  {:ok, result}

                :unknown ->
                  data_layer_result =
                    if Ash.DataLayer.data_layer_can?(resource, :calculate) do
                      Ash.DataLayer.calculate(resource, [expr], %{
                        calculation_context: calc_context,
                        primary_key: primary_key
                      })
                    else
                      :cant_calculate
                    end

                  case data_layer_result do
                    {:ok, result} ->
                      {:ok, Enum.at(result, 0)}

                    :cant_calculate ->
                      {:error,
                       "Failed to run calculation in memory, or in the data layer, and no `calculate/3` is defined on #{inspect(module)}. Data layer does not support one-off calculations."}

                    {:error, error} ->
                      if module.has_calculate?() do
                        case module.calculate([record], calc_opts, calc_context) do
                          [result] ->
                            result

                          {:ok, [result]} ->
                            {:ok, result}

                          {:ok, _} ->
                            {:error, "Invalid calculation return"}

                          {:error, error} ->
                            {:error, error}

                          :unknown ->
                            {:error,
                             "Failed to run calculation in memory, or in the data layer. Data layer returned #{inspect(error)}"}
                        end
                      else
                        {:error,
                         "Failed to run calculation in memory, or in the data layer, and no `calculate/3` is defined on #{inspect(module)}. Data layer returned #{inspect(error)}"}
                      end
                  end

                {:error, error} ->
                  {:error, error}
              end
            else
              case Ash.load(record, [{calculation, arguments}],
                     actor: opts[:actor],
                     domain: opts[:domain],
                     tenant: opts[:tenant],
                     authorize?: opts[:authorize?],
                     tracer: opts[:tracer],
                     resource: opts[:resource],
                     context: opts[:context] || %{}
                   ) do
                {:ok, record} -> {:ok, Map.get(record, calculation)}
                {:error, error} -> {:error, error}
              end
            end
          end
        end
      else
        if module.has_calculate?() do
          case module.calculate([record], calc_opts, calc_context) do
            [result] ->
              {:ok, result}

            {:ok, [result]} ->
              {:ok, result}

            {:ok, _} ->
              {:error, "Invalid calculation return"}

            {:error, error} ->
              {:error, error}
          end
        else
          {:error, "Module #{inspect(module)} does not have an expression or calculate function"}
        end
      end
    else
      {:calc, nil} ->
        {:error, "No such calculation"}

      {:error, error} ->
        {:error, error}
    end
  end

  defp requires_primary_key?(expr) do
    Ash.Filter.find_value(expr, fn
      %Ash.Query.Ref{attribute: %agg_struct{}}
      when agg_struct in [Ash.Query.Aggregate, Ash.Resource.Aggregate] ->
        true

      %Ash.Query.Exists{} ->
        true

      %Ash.Query.Parent{} ->
        true

      _ ->
        false
    end) ||
      false
  end

  defp replace_refs(expr, opts) do
    Ash.Filter.map(expr, fn
      %Ash.Query.Ref{relationship_path: path, attribute: %Ash.Resource.Attribute{} = attribute} ->
        name =
          case attribute do
            %{name: name} -> name
            name -> name
          end

        Ash.Expr.get_path(opts[:record] || %{}, path ++ [name])

      %Ash.Query.Exists{expr: expr} = exists ->
        %{
          exists
          | expr:
              Ash.Filter.map(expr, fn
                %Ash.Query.Parent{expr: expr} = parent ->
                  %{parent | expr: replace_refs(expr, opts)}

                other ->
                  other
              end)
        }

      other ->
        other
    end)
  end

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
          Ash.Context.to_opts(calculation.context),
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
      Enum.zip_with([records, values], fn
        [record, %Ash.NotLoaded{}] ->
          raise """
          Invalid return from calculation, expected a value, got `%Ash.NotLoaded{}`

          Calculation: #{inspect(calculation.name)}
          Record: #{inspect(record)}
          """

        [record, value] ->
          Map.put(record, calculation.load, value)
      end)
    else
      Enum.zip_with([records, values], fn
        [record, %Ash.NotLoaded{}] ->
          raise """
          Invalid return from calculation, expected a value, got `%Ash.NotLoaded{}`

          Calculation: #{inspect(calculation.name)}
          Record: #{inspect(record)}
          """

        [record, value] ->
          Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))
      end)
    end
  end

  defp run_calculation(calculation, ash_query, records) do
    context = Map.put(calculation.context, :domain, ash_query.domain)

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
  def rewrite(_rewrites, []), do: []

  def rewrite(rewrites, record) when not is_list(record) do
    rewrites
    |> rewrite([record])
    |> Enum.at(0)
  end

  def rewrite(rewrites, [%resource{} | _] = records) do
    rewrites
    |> Enum.sort_by(fn
      {{path, _, _, _}, _} ->
        Enum.count(path)

      _ ->
        :infinity
    end)
    |> Enum.reduce(records, fn
      {:cleanup_field_auth, further_load}, records ->
        query =
          resource
          |> Ash.Query.load(further_load)

        Ash.Actions.Read.cleanup_field_auth(records, query, false)

      {{path, data, calc_name, calc_load}, source}, records when path != [] ->
        rewrite_at_path(records, {{path, data, calc_name, calc_load}, source})

      {{_, {:calc, to_name, to_load}, _, _}, source}, records ->
        if to_load do
          Enum.map(records, fn record ->
            case record do
              %Ash.NotLoaded{} -> record
              %Ash.ForbiddenField{} -> record
              nil -> record
              record -> Map.put(record, to_load, Map.get(record.calculations, source))
            end
          end)
        else
          Enum.map(records, fn record ->
            case record do
              %Ash.NotLoaded{} ->
                record

              %Ash.ForbiddenField{} ->
                record

              nil ->
                record

              record ->
                Map.update!(
                  record,
                  :calculations,
                  &Map.put(&1, to_name, Map.get(record.calculations, source))
                )
            end
          end)
        end

      {{_, {:agg, to_name, to_load}, _, _}, source}, records ->
        if to_load do
          Enum.map(records, fn record ->
            case record do
              %Ash.NotLoaded{} -> record
              %Ash.ForbiddenField{} -> record
              nil -> record
              record -> Map.put(record, to_load, Map.get(record.aggregates, source))
            end
          end)
        else
          Enum.map(records, fn record ->
            case record do
              %Ash.NotLoaded{} ->
                record

              %Ash.ForbiddenField{} ->
                record

              nil ->
                record

              record ->
                Map.update!(
                  record,
                  :aggregates,
                  &Map.put(&1, to_name, Map.get(record.calculations, source))
                )
            end
          end)
        end

      {{path, {:rel, rel_name}, _, _}, source}, records ->
        update_at_path(records, path, fn record ->
          Map.put(record, rel_name, Map.get(record.calculations, source))
        end)
    end)
  end

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
  defp update_at_path(%Ash.NotLoaded{} = value, _, _), do: value
  defp update_at_path(%Ash.ForbiddenField{} = value, _, _), do: value

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

        if calculation.module.strict_loads? do
          []
        else
          relationship.destination
          |> Ash.Query.new()
          |> get_all_rewrites(calculation, path ++ [name])
        end

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

  def split_and_load_calculations(domain, ash_query, missing_pkeys?, initial_data, reuse_values?) do
    can_expression_calculation? =
      !missing_pkeys? &&
        Ash.DataLayer.data_layer_can?(ash_query.resource, :expression_calculation)

    ash_query =
      Enum.reduce(ash_query.calculations, ash_query, fn {_name, calc}, ash_query ->
        load_calculation_requirements(
          domain,
          ash_query,
          calc,
          can_expression_calculation?,
          initial_data,
          reuse_values?
        )
      end)

    ash_query.calculations
    |> Map.values()
    |> Enum.reduce({[], [], ash_query}, fn calculation, {in_query, at_runtime, ash_query} ->
      if calculation.module.has_expression?() do
        expression =
          calculation.opts
          |> calculation.module.expression(calculation.context)
          |> Ash.Expr.fill_template(
            calculation.context.actor,
            calculation.context.arguments,
            calculation.context.source_context
          )
          |> Ash.Actions.Read.add_calc_context_to_filter(
            calculation.context.actor,
            calculation.context.authorize?,
            calculation.context.tenant,
            calculation.context.tracer,
            domain
          )

        case try_evaluate(
               expression,
               ash_query.resource,
               calculation,
               initial_data,
               reuse_values?
             ) do
          {:ok, new_calculation} ->
            {in_query, [new_calculation | at_runtime], ash_query}

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

  defp try_evaluate(expression, resource, calculation, {:ok, initial_data}, reuse_values?) do
    expression
    |> Ash.Filter.list_refs(false, false, true, true)
    |> then(fn refs ->
      if refs == [] do
        true
      else
        if reuse_values? do
          Enum.all?(refs, fn ref ->
            # consider doing `lists: :any`?
            Ash.Resource.loaded?(initial_data, ref.relationship_path ++ [ref.attribute],
              strict?: true,
              type: :request
            )
          end)
        else
          false
        end
      end
    end)
    |> case do
      true ->
        Enum.reduce_while(initial_data, {:ok, []}, fn record, {:ok, results} ->
          case Ash.Expr.eval(expression,
                 record: record,
                 resource: resource,
                 unknown_on_unknown_refs?: true
               ) do
            {:ok, result} ->
              {:cont, {:ok, [result | results]}}

            _ ->
              {:halt, :error}
          end
        end)
        |> case do
          {:ok, values} ->
            {:ok,
             %{
               calculation
               | module: Ash.Resource.Calculation.Literal,
                 opts: [value: Enum.reverse(values), precomputed?: true],
                 required_loads: [],
                 select: []
             }}

          _ ->
            :error
        end

      false ->
        :error
    end
  end

  defp try_evaluate(expression, resource, calculation, _, _) do
    case Ash.Expr.eval(expression,
           resource: resource,
           unknown_on_unknown_refs?: true
         ) do
      {:ok, result} ->
        {:ok,
         %{
           calculation
           | module: Ash.Resource.Calculation.Literal,
             opts: [value: result],
             required_loads: [],
             select: []
         }}

      _ ->
        :error
    end
  end

  defp all_referenced_calcs_support_expressions?(calculation, expression \\ nil, ash_query) do
    expression =
      expression ||
        calculation.opts
        |> calculation.module.expression(calculation.context)
        |> Ash.Expr.fill_template(
          calculation.context.actor,
          calculation.context.arguments,
          calculation.context.source_context
        )
        |> Ash.Actions.Read.add_calc_context_to_filter(
          calculation.context.actor,
          calculation.context.authorize?,
          calculation.context.tenant,
          calculation.context.tracer,
          ash_query.domain
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

          {:error, _error} ->
            true

          :error ->
            true
        end
    end
  end

  defp load_calculation_requirements(
         domain,
         query,
         calculation,
         can_expression_calculation?,
         initial_data,
         reuse_values?,
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
          domain,
          query,
          calculation.name,
          calculation.load,
          calc_path,
          calculation.module.strict_loads?(),
          relationship_path,
          can_expression_calculation?,
          [{calculation.module, calculation.opts} | checked_calculations],
          initial_data,
          reuse_values?
        )
      end
    end
  end

  defp do_load_calculation_requirements(
         requirements,
         domain,
         query,
         calc_name,
         calc_load,
         calc_path,
         strict_loads?,
         relationship_path,
         can_expression_calculation?,
         checked_calculations,
         initial_data,
         reuse_values?
       ) do
    requirements
    |> Enum.map(fn
      {key, value} ->
        {key, value}

      key ->
        {key, []}
    end)
    |> Enum.reduce(
      query,
      &load_single_calculation_dependency(
        &1,
        &2,
        domain,
        calc_name,
        calc_load,
        calc_path,
        strict_loads?,
        relationship_path,
        can_expression_calculation?,
        checked_calculations,
        initial_data,
        reuse_values?
      )
    )
  end

  defp load_single_calculation_dependency(
         {load, further},
         query,
         domain,
         calc_name,
         calc_load,
         calc_path,
         strict_loads?,
         relationship_path,
         can_expression_calculation?,
         checked_calculations,
         initial_data,
         reuse_values?
       ) do
    cond do
      match?(%Ash.Query.Calculation{}, load) ->
        load_depended_on_calc(
          query,
          load,
          further,
          domain,
          calc_name,
          calc_load,
          calc_path,
          relationship_path,
          can_expression_calculation?,
          checked_calculations,
          initial_data,
          strict_loads?,
          reuse_values?
        )

      match?(%Ash.Query.Aggregate{}, load) ->
        if loaded_and_reusable?(initial_data, relationship_path, load, reuse_values?) do
          query
        else
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
                  equivalent_aggregate.type,
                  {Ash.Resource.Calculation.FetchAgg,
                   load: equivalent_aggregate.name, name: equivalent_aggregate.load},
                  %{},
                  equivalent_aggregate.constraints,
                  equivalent_aggregate.context
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
        end

      attr = Ash.Resource.Info.attribute(query.resource, load) ->
        depended_on_fields = query.context[:private][:depended_on_fields] || []

        query =
          Ash.Query.set_context(query, %{
            private: %{depended_on_fields: [attr.name | depended_on_fields]}
          })

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
                    domain,
                    calc_name,
                    calc_load,
                    calc_path,
                    relationship_path,
                    initial_data,
                    strict_loads?,
                    reuse_values?
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
        if loaded_and_reusable?(initial_data, relationship_path, load, reuse_values?) do
          query
        else
          Ash.Query.load(query, agg.name)
        end

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
                 resource_calculation.type,
                 resource_calculation.constraints,
                 arguments: args,
                 filterable?: resource_calculation.filterable?,
                 sortable?: resource_calculation.sortable?,
                 sensitive?: resource_calculation.sensitive?,
                 load: resource_calculation.load,
                 source_context: query.context
               ) do
          calculation =
            Ash.Query.select_and_load_calc(
              resource_calculation,
              %{calculation | load: load, calc_name: resource_calculation.name},
              query
            )

          load_single_calculation_dependency(
            {calculation, load_through},
            query,
            domain,
            calc_name,
            calc_load,
            calc_path,
            strict_loads?,
            relationship_path,
            can_expression_calculation?,
            checked_calculations,
            initial_data,
            reuse_values?
          )
        else
          {:error, error} ->
            Ash.Query.add_error(query, :load, error)
        end

      relationship = Ash.Resource.Info.relationship(query.resource, load) ->
        query = Ash.Query.ensure_selected(query, relationship.source_attribute)

        further = to_loaded_query(relationship.destination, further, strict_loads?)

        if loaded_and_reusable?(initial_data, relationship_path, relationship, reuse_values?) do
          case query.load[relationship.name] do
            nil ->
              related_query =
                relationship.destination
                |> Ash.Query.set_context(%{private: %{lazy?: true}})
                |> merge_query_load(
                  further,
                  relationship.domain || domain,
                  calc_path,
                  calc_name,
                  calc_load,
                  relationship_path ++ [relationship.name],
                  initial_data,
                  strict_loads?,
                  reuse_values?
                )

              Ash.Query.load(query, [{relationship.name, related_query}])

            related_query ->
              related_query =
                merge_query_load(
                  related_query,
                  further,
                  relationship.domain || domain,
                  calc_path,
                  calc_name,
                  calc_load,
                  relationship_path ++ [relationship.name],
                  initial_data,
                  strict_loads?,
                  reuse_values?
                )

              Ash.Query.load(query, [{relationship.name, related_query}])
          end
        else
          case query.load[relationship.name] do
            nil ->
              Ash.Query.load(query, [{relationship.name, further}])

            current_load ->
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
                          relationship.domain || domain,
                          calc_path,
                          calc_name,
                          calc_load,
                          relationship_path ++ [relationship.name],
                          initial_data,
                          strict_loads?,
                          reuse_values?
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
                      type,
                      {Ash.Resource.Calculation.LoadRelationship,
                       relationship: relationship.name,
                       query: further,
                       domain: relationship.domain || domain},
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
                            relationship.domain || domain,
                            calc_path,
                            calc_name,
                            calc_load,
                            relationship_path ++ [relationship.name],
                            initial_data,
                            strict_loads?,
                            reuse_values?
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
        end

      true ->
        raise "unknown load for #{inspect(query)}: #{inspect(load)}"
    end
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
         domain,
         calc_name,
         calc_load,
         calc_path,
         relationship_path,
         can_expression_calculation?,
         checked_calculations,
         initial_data,
         strict_loads?,
         reuse_values?
       ) do
    if loaded_and_reusable?(initial_data, relationship_path, calculation, reuse_values?) do
      do_merge_load_through(
        query,
        further,
        :calculation,
        calculation.name,
        calculation.type,
        calculation.constraints,
        domain,
        calc_name,
        calc_load,
        calc_path,
        relationship_path,
        initial_data,
        strict_loads?,
        reuse_values?
      )
    else
      case find_equivalent_calculation(query, calculation) do
        {:ok, equivalent_calculation} ->
          if equivalent_calculation.load == calculation.load and
               equivalent_calculation.name == calculation.name do
            query =
              do_merge_load_through(
                query,
                further,
                :calculation,
                equivalent_calculation.name,
                equivalent_calculation.type,
                equivalent_calculation.constraints,
                domain,
                calc_name,
                calc_load,
                calc_path,
                relationship_path,
                initial_data,
                strict_loads?,
                reuse_values?
              )

            add_calculation_dependency(query, calc_name, equivalent_calculation.name)
          else
            query =
              do_merge_load_through(
                query,
                further,
                :calculation,
                calculation.name,
                calculation.type,
                calculation.constraints,
                domain,
                calc_name,
                calc_load,
                calc_path,
                relationship_path,
                initial_data,
                strict_loads?,
                reuse_values?
              )

            new_calc_name =
              {:__calc_dep__,
               [
                 {calc_path, {:calc, equivalent_calculation.name, equivalent_calculation.load},
                  calc_name, calc_load}
               ]}

            Ash.Query.calculate(
              query,
              new_calc_name,
              equivalent_calculation.type,
              {Ash.Resource.Calculation.FetchCalc,
               load: equivalent_calculation.name, name: equivalent_calculation.load},
              equivalent_calculation.context.arguments,
              equivalent_calculation.constraints,
              equivalent_calculation.context
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

          domain
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
  end

  defp do_merge_load_through(
         query,
         further,
         load_type,
         name,
         type,
         constraints,
         domain,
         calc_name,
         calc_load,
         calc_path,
         relationship_path,
         initial_data,
         strict_loads?,
         reuse_values?
       ) do
    case Map.fetch(
           query.load_through[load_type] || %{},
           name
         ) do
      :error ->
        if further in [nil, []] do
          query
        else
          load_through =
            query.load_through
            |> Map.put_new(load_type, %{})
            |> Map.update!(
              load_type,
              &Map.put(&1, name, further)
            )

          %{query | load_through: load_through}
        end

      {:ok, value} ->
        load_through =
          Map.update!(query.load_through, load_type, fn type_load_through ->
            Map.put(
              type_load_through,
              name,
              merge_load_through(
                value || [],
                further || [],
                type,
                constraints,
                domain,
                calc_name,
                calc_load,
                calc_path,
                relationship_path,
                initial_data,
                strict_loads?,
                reuse_values?
              )
            )
          end)

        %{
          query
          | load_through: load_through
        }
    end
  end

  defp loaded_and_reusable?({:ok, initial_data}, relationship_path, calculation, true) do
    Ash.Resource.loaded?(initial_data, relationship_path ++ [calculation],
      strict?: true,
      type: :request
    )
  end

  defp loaded_and_reusable?(_initial_data, _relationship_path, _calculation, _false), do: false

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
           other_calc.context.arguments == calculation.context.arguments do
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
         domain,
         calc_name,
         calc_load,
         calc_path,
         relationship_path,
         initial_data,
         strict_loads?,
         reuse_values?
       ) do
    case Ash.Type.merge_load(type, old, new, constraints, %{
           domain: domain,
           calc_name: calc_name,
           calc_load: calc_load,
           calc_path: calc_path,
           relationship_path: relationship_path,
           initial_data: initial_data,
           reuse_values?: reuse_values?,
           strict_loads?: strict_loads?
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

  def merge_query_load(
        left,
        right,
        domain,
        calc_path,
        calc_name,
        calc_load,
        relationship_path,
        initial_data \\ :error,
        strict_loads? \\ false,
        reuse_values? \\ false
      ) do
    can_expression_calculation? =
      Ash.DataLayer.data_layer_can?(left.resource, :expression_calculation)

    do_load_calculation_requirements(
      right.load ++
        Map.values(right.calculations) ++
        Map.values(right.aggregates) ++ query_select(right, strict_loads?),
      domain,
      left,
      calc_name,
      calc_load,
      calc_path,
      strict_loads?,
      relationship_path,
      can_expression_calculation?,
      [],
      initial_data,
      reuse_values?
    )
  end

  defp query_select(%{resource: resource, select: nil}, false) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.map(& &1.name)
  end

  defp query_select(%{select: select}, _), do: List.wrap(select)

  defp to_loaded_query(resource, %Ash.Query{resource: resource} = query, strict_loads?) do
    if strict_loads? do
      Ash.Query.select(query, [])
    else
      query
    end
  end

  defp to_loaded_query(resource, loads, strict_loads?) do
    if strict_loads? do
      resource
      |> Ash.Query.select([])
      |> Ash.Query.load(apply_strict(resource, loads, strict_loads?))
    else
      resource
      |> Ash.Query.load(loads)
    end
  end

  defp apply_strict(resource, loads, strict_loads?) do
    loads
    |> List.wrap()
    |> Enum.map(fn
      {key, value} ->
        if relationship = Ash.Resource.Info.relationship(resource, key) do
          {key, to_loaded_query(relationship.destination, value, strict_loads?)}
        else
          {key, value}
        end

      key ->
        if relationship = Ash.Resource.Info.relationship(resource, key) do
          {key, to_loaded_query(relationship.destination, [], strict_loads?)}
        else
          key
        end
    end)
  end

  # Deselect fields that we know statically cannot be seen
  # The field may be reselected later as a calculation dependency
  # this is an optimization not a guarantee
  def deselect_known_forbidden_fields(ash_query, calculations_at_runtime) do
    depended_on_fields = ash_query.context[:private][:depended_on_fields] || []

    calculations_at_runtime
    |> Enum.reduce([], fn
      %{
        name: {:__ash_fields_are_visible__, fields},
        module: module,
        opts: opts
      },
      deselect_fields
      when module in [
             Ash.Resource.Calculation.Expression,
             Ash.Resource.Calculation.RuntimeExpression,
             Ash.Resource.Calculation.Literal
           ] ->
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
    |> Enum.uniq()
    |> Kernel.--(depended_on_fields)
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
