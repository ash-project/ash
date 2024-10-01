defmodule Ash.DataLayer.Ets do
  @behaviour Ash.DataLayer
  require Ash.Query
  import Ash.Expr
  require Logger

  @ets %Spark.Dsl.Section{
    name: :ets,
    describe: """
    A section for configuring the ets data layer
    """,
    examples: [
      """
      ets do
        # Used in testing
        private? true
      end
      """
    ],
    schema: [
      private?: [
        type: :boolean,
        default: false,
        doc:
          "Sets the ets table protection to private, and scopes it to only this process. The table name will not be used directly if this is true, to allow multiple processes to use this resource separately."
      ],
      table: [
        type: :atom,
        doc: """
        The name of the table. Defaults to the resource name.
        """
      ]
    ]
  }

  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing and lightweight usage.

  Remember, this does not have support for transactions! This is not recommended for production
  use, especially in multi-user applications. It can, however, be great for prototyping.
  """

  use Spark.Dsl.Extension,
    sections: [@ets],
    verifiers: [Ash.DataLayer.Verifiers.RequirePreCheckWith]

  alias Ash.Actions.Sort

  defmodule Query do
    @moduledoc false
    defstruct [
      :resource,
      :filter,
      :limit,
      :sort,
      :tenant,
      :domain,
      :distinct,
      :distinct_sort,
      context: %{},
      calculations: [],
      aggregates: [],
      relationships: %{},
      offset: 0
    ]
  end

  defmodule TableManager do
    @moduledoc false
    use GenServer

    def start(resource, tenant) do
      table =
        if tenant && Ash.Resource.Info.multitenancy_strategy(resource) == :context do
          Module.concat(to_string(Ash.DataLayer.Ets.Info.table(resource)), to_string(tenant))
        else
          Ash.DataLayer.Ets.Info.table(resource)
        end

      if Ash.DataLayer.Ets.Info.private?(resource) do
        do_wrap_existing(resource, table)
      else
        case GenServer.start(__MODULE__, {resource, table},
               name: Module.concat(table, TableManager)
             ) do
          {:error, {:already_started, _pid}} ->
            ETS.Set.wrap_existing(table)

          {:error, error} ->
            {:error, error}

          _ ->
            ETS.Set.wrap_existing(table)
        end
      end
    end

    def init({resource, table}) do
      case do_wrap_existing(resource, table) do
        {:ok, table} ->
          {:ok, {resource, table}, :hibernate}

        {:error, error} ->
          {:error, error}
      end
    end

    def handle_call(:wait, _, state), do: {:reply, :ok, state}

    defp do_wrap_existing(_resource, table) do
      case ETS.Set.wrap_existing(table) do
        {:error, :table_not_found} ->
          case ETS.Set.new(
                 name: table,
                 protection: :public,
                 ordered: true,
                 read_concurrency: true
               ) do
            {:ok, tab} ->
              {:ok, tab}

            {:error, :table_already_exists} ->
              ETS.Set.wrap_existing(table)

            other ->
              other
          end

        {:ok, table} ->
          {:ok, table}

        {:error, other} ->
          {:error, other}
      end
    end
  end

  @doc "Stops the storage for a given resource/tenant (deleting all of the data)"
  # sobelow_skip ["DOS.StringToAtom"]
  def stop(resource, tenant \\ nil) do
    tenant =
      if Ash.Resource.Info.multitenancy_strategy(resource) == :context do
        tenant
      end

    if Ash.DataLayer.Ets.Info.private?(resource) do
      case Process.get({:ash_ets_table, resource, tenant}) do
        nil ->
          :ok

        table ->
          ETS.Set.delete(table)
      end
    else
      table =
        if tenant && Ash.Resource.Info.multitenancy_strategy(resource) == :context do
          String.to_atom(to_string(tenant) <> to_string(resource))
        else
          resource
        end

      name = Module.concat(table, TableManager)

      case Process.whereis(name) do
        nil ->
          :ok

        pid ->
          Process.exit(pid, :shutdown)
      end
    end
  end

  @doc false
  @impl true
  def can?(_, :distinct_sort), do: true
  def can?(resource, :async_engine), do: not Ash.DataLayer.Ets.Info.private?(resource)
  def can?(_, {:lateral_join, _}), do: true
  def can?(_, :bulk_create), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :expression_calculation), do: true
  def can?(_, :expression_calculation_sort), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, :upsert), do: true
  def can?(_, :calculate), do: true
  def can?(_, :aggregate_filter), do: true
  def can?(_, :aggregate_sort), do: true
  def can?(_, {:aggregate_relationship, _}), do: true
  def can?(_, {:filter_relationship, _}), do: true
  def can?(_, {:aggregate, :count}), do: true
  def can?(_, {:aggregate, :first}), do: true
  def can?(_, {:aggregate, :sum}), do: true
  def can?(_, {:aggregate, :list}), do: true
  def can?(_, {:aggregate, :max}), do: true
  def can?(_, {:aggregate, :min}), do: true
  def can?(_, {:aggregate, :avg}), do: true
  def can?(_, {:aggregate, :exists}), do: true
  def can?(_, :changeset_filter), do: true
  def can?(_, :update_query), do: true
  def can?(_, :destroy_query), do: true
  def can?(resource, {:query_aggregate, kind}), do: can?(resource, {:aggregate, kind})

  def can?(_, :create), do: true
  def can?(_, :read), do: true

  def can?(resource, action_type) when action_type in ~w[update destroy]a do
    resource
    |> Ash.Resource.Info.primary_key()
    |> Enum.any?()
  end

  def can?(_, :sort), do: true
  def can?(_, :filter), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :distinct), do: true
  def can?(_, :transact), do: false
  def can?(_, {:filter_expr, _}), do: true

  case Application.compile_env(:ash, :no_join_mnesia_ets) || false do
    false ->
      def can?(_, {:join, _resource}) do
        # we synthesize all filters under the hood using `Ash.Filter.Runtime`
        true
      end

    true ->
      def can?(_, {:join, _resource}) do
        # we synthesize all filters under the hood using `Ash.Filter.Runtime`
        false
      end

    :dynamic ->
      def can?(_, {:join, resource}) do
        Ash.Resource.Info.data_layer(resource) == __MODULE__ ||
          Application.get_env(:ash, :mnesia_ets_join?, true)
      end
  end

  def can?(_, :nested_expressions), do: true
  def can?(_, {:query_aggregate, :count}), do: true
  def can?(_, {:query_aggregate, :first}), do: true
  def can?(_, {:query_aggregate, :sum}), do: true
  def can?(_, {:query_aggregate, :list}), do: true
  def can?(_, {:query_aggregate, :max}), do: true
  def can?(_, {:query_aggregate, :min}), do: true
  def can?(_, {:query_aggregate, :avg}), do: true
  def can?(_, {:query_aggregate, :exists}), do: true
  def can?(_, :expr_error), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, {:atomic, :update}), do: true
  def can?(_, {:atomic, :upsert}), do: true
  def can?(_, _), do: false

  @doc false
  @impl true
  def resource_to_query(resource, domain) do
    %Query{
      resource: resource,
      domain: domain
    }
  end

  @doc false
  @impl true
  def limit(query, limit, _), do: {:ok, %{query | limit: limit}}

  @doc false
  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

  @doc false
  @impl true
  def add_calculations(query, calculations, _) do
    {:ok, %{query | calculations: query.calculations ++ calculations}}
  end

  @doc false
  @impl true
  def add_aggregate(query, aggregate, _),
    do: {:ok, %{query | aggregates: [aggregate | query.aggregates]}}

  @doc false
  @impl true
  def set_tenant(_resource, query, tenant) do
    {:ok, %{query | tenant: tenant}}
  end

  @doc false
  @impl true
  def set_context(_resource, query, context) do
    {:ok, %{query | context: context}}
  end

  @doc false
  @impl true
  def filter(query, filter, _resource) do
    if query.filter do
      {:ok, %{query | filter: Ash.Filter.add_to_filter!(query.filter, filter)}}
    else
      {:ok, %{query | filter: filter}}
    end
  end

  @doc false
  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @doc false
  @impl true
  def distinct(query, distinct, _resource) do
    {:ok, %{query | distinct: distinct}}
  end

  @impl true
  def distinct_sort(query, distinct_sort, _resource) do
    {:ok, %{query | distinct_sort: distinct_sort}}
  end

  @doc false
  @impl true
  def run_aggregate_query(%{domain: domain} = query, aggregates, resource) do
    case run_query(query, resource) do
      {:ok, results} ->
        Enum.reduce_while(aggregates, {:ok, %{}}, fn
          %{
            kind: kind,
            name: name,
            query: query,
            field: field,
            resource: resource,
            uniq?: uniq?,
            include_nil?: include_nil?,
            default_value: default_value,
            context: context
          },
          {:ok, acc} ->
            results
            |> filter_matches(
              Map.get(query || %{}, :filter),
              domain,
              context[:tenant],
              context[:actor]
            )
            |> case do
              {:ok, matches} ->
                field = field || Enum.at(Ash.Resource.Info.primary_key(resource), 0)

                value = aggregate_value(matches, kind, field, uniq?, include_nil?, default_value)
                {:cont, {:ok, Map.put(acc, name, value)}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
        end)

      {:error, error} ->
        {:error, error}
    end
    |> case do
      {:error, error} ->
        {:error, error}

      other ->
        other
    end
  end

  @doc false
  @impl true
  def run_query(
        %Query{
          resource: resource,
          filter: filter,
          offset: offset,
          limit: limit,
          sort: sort,
          distinct: distinct,
          distinct_sort: distinct_sort,
          tenant: tenant,
          calculations: calculations,
          aggregates: aggregates,
          domain: domain,
          context: context
        },
        _resource,
        parent \\ nil
      ) do
    with {:ok, records} <- get_records(resource, tenant),
         {:ok, records} <-
           filter_matches(
             records,
             filter,
             domain,
             context[:private][:tenant],
             context[:private][:actor],
             parent
           ),
         records <- Sort.runtime_sort(records, distinct_sort || sort, domain: domain),
         records <- Sort.runtime_distinct(records, distinct, domain: domain),
         records <- Sort.runtime_sort(records, sort, domain: domain),
         records <- Enum.drop(records, offset || []),
         records <- do_limit(records, limit),
         {:ok, records} <-
           do_add_aggregates(records, domain, resource, aggregates),
         {:ok, records} <-
           do_add_calculations(records, resource, calculations, domain) do
      {:ok, records}
    else
      {:error, error} ->
        {:error, error}
    end
  end

  defp do_limit(records, nil), do: records
  defp do_limit(records, limit), do: Enum.take(records, limit)

  @impl true
  def prefer_lateral_join_for_many_to_many?, do: false

  @impl true
  def run_query_with_lateral_join(
        query,
        root_data,
        _destination_resource,
        [
          {source_query, source_attribute, destination_attribute, relationship}
        ]
      ) do
    source_query =
      source_query
      |> Ash.Query.unset(:load)
      |> Ash.Query.unset(:page)
      |> Ash.Query.set_context(%{private: %{internal?: true}})
      |> Ash.Query.set_domain(query.domain)

    primary_key = Ash.Resource.Info.primary_key(source_query.resource)

    source_query =
      case primary_key do
        [] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, source_attribute))

          Ash.Query.filter(source_query, ^ref(source_attribute) in ^source_attributes)

        [field] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, field))
          Ash.Query.filter(source_query, ^ref(field) in ^source_attributes)

        fields ->
          filter = [
            or:
              Enum.map(root_data, fn record ->
                [and: Map.take(record, fields) |> Map.to_list()]
              end)
          ]

          Ash.Query.do_filter(source_query, filter)
      end

    source_query
    |> Ash.Actions.Read.unpaginated_read(nil, authorize?: false)
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, root_data} ->
        root_data
        |> Enum.reduce_while({:ok, []}, fn parent, {:ok, results} ->
          new_filter =
            if Map.get(relationship, :no_attributes?) do
              query.filter
            else
              filter =
                if is_nil(query.filter) do
                  %Ash.Filter{resource: query.resource, expression: true}
                else
                  query.filter
                end

              Ash.Filter.add_to_filter!(
                filter,
                Ash.Filter.parse!(
                  query.resource,
                  Ash.Expr.expr(^ref(destination_attribute) == ^Map.get(parent, source_attribute))
                )
              )
            end

          query = %{query | filter: new_filter}

          case run_query(query, relationship.source, parent) do
            {:ok, new_results} ->
              new_results =
                Enum.map(
                  new_results,
                  &Map.put(&1, :__lateral_join_source__, Map.take(parent, primary_key))
                )

              {:cont, {:ok, new_results ++ results}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
    end
  end

  def run_query_with_lateral_join(query, root_data, _destination_resource, [
        {source_query, source_attribute, source_attribute_on_join_resource, relationship},
        {through_query, destination_attribute_on_join_resource, destination_attribute,
         _through_relationship}
      ]) do
    source_query =
      source_query
      |> Ash.Query.unset(:load)
      |> Ash.Query.unset(:page)
      |> Ash.Query.set_context(%{private: %{internal?: true}})
      |> Ash.Query.set_domain(query.domain)

    primary_key = Ash.Resource.Info.primary_key(source_query.resource)

    source_query =
      case primary_key do
        [] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, source_attribute))

          Ash.Query.filter(source_query, ^ref(source_attribute) in ^source_attributes)

        [field] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, field))
          Ash.Query.filter(source_query, ^ref(field) in ^source_attributes)

        fields ->
          filter = [
            or:
              Enum.map(root_data, fn record ->
                [and: Map.take(record, fields) |> Map.to_list()]
              end)
          ]

          Ash.Query.do_filter(source_query, filter)
      end

    source_query
    |> Ash.read(authorize?: false)
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, root_data} ->
        root_data
        |> Enum.reduce_while({:ok, []}, fn parent, {:ok, results} ->
          through_query
          |> Ash.Query.filter(
            ^ref(source_attribute_on_join_resource) ==
              ^Map.get(parent, source_attribute)
          )
          |> Ash.Query.set_context(%{private: %{internal?: true}})
          |> Ash.Query.set_domain(query.domain)
          |> Ash.read(authorize?: false)
          |> case do
            {:ok, join_data} ->
              join_attrs =
                Enum.map(join_data, &Map.get(&1, destination_attribute_on_join_resource))

              new_filter =
                if is_nil(query.filter) do
                  Ash.Filter.parse!(query.resource, [
                    {destination_attribute, [in: join_attrs]}
                  ])
                else
                  Ash.Filter.add_to_filter!(query.filter, [
                    {destination_attribute, [in: join_attrs]}
                  ])
                end

              query = %{query | filter: new_filter}

              case run_query(query, relationship.source, parent) do
                {:ok, new_results} ->
                  new_results =
                    Enum.flat_map(new_results, fn result ->
                      join_data
                      |> Enum.flat_map(fn join_row ->
                        # TODO: use `Ash.Type.equal?`
                        if Map.get(join_row, destination_attribute_on_join_resource) ==
                             Map.get(result, destination_attribute) do
                          [
                            Map.put(
                              result,
                              :__lateral_join_source__,
                              Map.take(parent, primary_key)
                            )
                          ]
                        else
                          []
                        end
                      end)
                    end)

                  {:cont, {:ok, new_results ++ results}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
    end
  end

  def do_add_calculations(records, _resource, [], _domain), do: {:ok, records}

  def do_add_calculations(records, resource, calculations, domain) do
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      calculations
      |> Enum.reduce_while({:ok, record}, fn {calculation, expression}, {:ok, record} ->
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               public?: false
             }) do
          {:ok, expression} ->
            expression =
              Ash.Actions.Read.add_calc_context_to_filter(
                expression,
                calculation.context.actor,
                calculation.context.authorize?,
                calculation.context.tenant,
                calculation.context.tracer,
                domain
              )

            case Ash.Expr.eval_hydrated(expression,
                   record: record,
                   resource: resource,
                   domain: domain,
                   actor: calculation.context.actor,
                   tenant: calculation.context.tenant
                 ) do
              {:ok, value} ->
                if calculation.load do
                  {:cont, {:ok, Map.put(record, calculation.load, value)}}
                else
                  {:cont,
                   {:ok,
                    Map.update!(
                      record,
                      :calculations,
                      &Map.put(&1, calculation.name, value)
                    )}}
                end

              :unknown ->
                if calculation.load do
                  {:cont, {:ok, Map.put(record, calculation.load, nil)}}
                else
                  {:cont,
                   {:ok,
                    Map.update!(
                      record,
                      :calculations,
                      &Map.put(&1, calculation.name, nil)
                    )}}
                end

              {:error, error} ->
                {:halt, {:error, error}}
            end

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, record} ->
          {:cont, {:ok, [record | records]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, records} ->
        {:ok, Enum.reverse(records)}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def do_add_aggregates(records, _domain, _resource, []), do: {:ok, records}

  def do_add_aggregates(records, domain, _resource, aggregates) do
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      aggregates
      |> Enum.reduce_while(
        {:ok, record},
        fn
          %{
            kind: kind,
            field: field,
            relationship_path: relationship_path,
            query: query,
            name: name,
            load: load,
            uniq?: uniq?,
            include_nil?: include_nil?,
            context: context,
            default_value: default_value,
            join_filters: join_filters
          },
          {:ok, record} ->
            with {:ok, loaded_record} <-
                   Ash.load(
                     record,
                     record.__struct__
                     |> Ash.Query.load(relationship_path_to_load(relationship_path, field))
                     |> Ash.Query.set_context(%{private: %{internal?: true}}),
                     domain: domain,
                     tenant: context[:tenant],
                     actor: context[:actor],
                     authorize?: false
                   ),
                 related <-
                   Ash.Filter.Runtime.get_related(
                     loaded_record,
                     relationship_path,
                     false,
                     join_filters,
                     [record],
                     domain
                   ),
                 {:ok, filtered} <-
                   filter_matches(
                     related,
                     query.filter,
                     domain,
                     context[:tenant],
                     context[:actor]
                   ),
                 sorted <- Sort.runtime_sort(filtered, query.sort, domain: domain) do
              field = field || Enum.at(Ash.Resource.Info.primary_key(query.resource), 0)

              value =
                aggregate_value(sorted, kind, field, uniq?, include_nil?, default_value)

              if load do
                {:cont, {:ok, Map.put(record, load, value)}}
              else
                {:cont, {:ok, Map.update!(record, :aggregates, &Map.put(&1, name, value))}}
              end
            else
              other ->
                {:halt, other}
            end
        end
      )
      |> case do
        {:ok, record} ->
          {:cont, {:ok, [record | records]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, records} ->
        {:ok, Enum.reverse(records)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp relationship_path_to_load([], leaf) do
    leaf
  end

  defp relationship_path_to_load([key | rest], leaf) do
    [{key, relationship_path_to_load(rest, leaf)}]
  end

  @doc false
  def aggregate_value(records, kind, field, uniq?, include_nil?, default) do
    case kind do
      :count ->
        if uniq? do
          records
          |> Stream.map(&field_value(&1, field))
          |> Stream.uniq()
          |> Stream.reject(&is_nil/1)
          |> Enum.count()
        else
          Enum.count(records, &(not is_nil(field_value(&1, field))))
        end

      :exists ->
        case records do
          [] ->
            false

          _ ->
            true
        end

      :first ->
        if include_nil? do
          case records do
            [] ->
              default

            [record | _rest] ->
              field_value(record, field)
          end
        else
          Enum.find_value(records, fn record ->
            case field_value(record, field) do
              nil ->
                nil

              value ->
                {:value, value}
            end
          end)
          |> case do
            nil -> nil
            {:value, value} -> value
          end
        end

      :list ->
        records
        |> Enum.map(fn record ->
          field_value(record, field)
        end)
        |> then(fn values ->
          if include_nil? do
            values
          else
            Enum.reject(values, &is_nil/1)
          end
        end)
        |> then(fn values ->
          if uniq? do
            Enum.uniq(values)
          else
            values
          end
        end)

      :avg ->
        records
        |> then(fn records ->
          if uniq? do
            records
            |> Stream.map(&field_value(&1, field))
            |> Stream.uniq()
          else
            records
            |> Stream.map(&field_value(&1, field))
          end
        end)
        |> Enum.reduce({nil, 0}, fn value, {sum, count} ->
          case value do
            nil ->
              {sum, count}

            value ->
              case {sum, value} do
                {nil, %Decimal{}} ->
                  {Decimal.new(value), count + 1}

                {_not_nil, %Decimal{}} ->
                  {Decimal.add(sum, value), count + 1}

                {nil, _not_decimal} ->
                  {value, count + 1}

                {_not_nil, _not_decimal} ->
                  {sum + value, count + 1}
              end
          end
        end)
        |> case do
          {_, 0} ->
            nil

          {%Decimal{} = sum, count} ->
            Decimal.div(sum, count)

          {sum, count} ->
            sum / count
        end

      kind when kind in [:sum, :max, :min] ->
        records
        |> Enum.map(&field_value(&1, field))
        |> case do
          [] ->
            nil

          items ->
            items =
              if uniq? do
                items |> Stream.uniq() |> Stream.reject(&is_nil/1)
              else
                items |> Stream.reject(&is_nil/1)
              end

            first_item = List.first(Enum.to_list(Stream.take(items, 1)))

            case kind do
              :sum ->
                if is_struct(first_item, Decimal) do
                  Enum.reduce(items, Decimal.new(0), &Decimal.add(&1, &2))
                else
                  Enum.sum(items)
                end

              :max ->
                if is_struct(first_item, Decimal) do
                  Enum.reduce(items, &Decimal.max(&1, &2))
                else
                  Enum.max(items)
                end

              :min ->
                if is_struct(first_item, Decimal) do
                  Enum.reduce(items, &Decimal.min(&1, &2))
                else
                  Enum.min(items)
                end
            end
        end
    end
  end

  defp field_value(nil, _), do: nil

  defp field_value(record, field) when is_atom(field) do
    Map.get(record, field)
  end

  defp field_value(record, %struct{load: load, name: name})
       when struct in [Ash.Query.Aggregate, Ash.Query.Calculation] do
    if load do
      Map.get(record, load)
    else
      case struct do
        Ash.Query.Aggregate ->
          Map.get(record.aggregates, name)

        Ash.Query.Calculation ->
          Map.get(record.calculations, name)
      end
    end
  end

  defp field_value(record, %{name: name}) do
    Map.get(record, name)
  end

  defp get_records(resource, tenant) do
    with {:ok, table} <- wrap_or_create_table(resource, tenant),
         {:ok, record_tuples} <- ETS.Set.to_list(table),
         records <- Enum.map(record_tuples, &elem(&1, 1)) do
      cast_records(records, resource)
    end
  end

  @doc false
  def cast_records(records, resource) do
    records
    |> Enum.reduce_while({:ok, []}, fn record, {:ok, casted} ->
      case cast_record(record, resource) do
        {:ok, casted_record} ->
          {:cont, {:ok, [casted_record | casted]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, records} ->
        {:ok, Enum.reverse(records)}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def cast_record(record, resource) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reduce_while({:ok, %{}}, fn attribute, {:ok, attrs} ->
      case Map.get(record, attribute.name) do
        nil ->
          {:cont, {:ok, Map.put(attrs, attribute.name, nil)}}

        value ->
          case Ash.Type.cast_stored(attribute.type, value, attribute.constraints) do
            {:ok, value} ->
              {:cont, {:ok, Map.put(attrs, attribute.name, value)}}

            :error ->
              {:halt,
               {:error, "Failed to load #{inspect(value)} as type #{inspect(attribute.type)}"}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
      end
    end)
    |> case do
      {:ok, attrs} ->
        {:ok,
         %{
           struct(resource, attrs)
           | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}
         }}

      {:error, error} ->
        {:error, error}
    end
  end

  defp filter_matches(
         records,
         filter,
         domain,
         _tenant,
         actor,
         parent \\ nil,
         conflicting_upsert_values \\ nil
       )

  defp filter_matches([], _, _domain, _tenant, _actor, _parent, _conflicting_upsert_values),
    do: {:ok, []}

  defp filter_matches(
         records,
         nil,
         _domain,
         _tenant,
         _actor,
         _parent,
         _conflicting_upsert_values
       ),
       do: {:ok, records}

  defp filter_matches(
         records,
         filter,
         domain,
         tenant,
         actor,
         parent,
         conflicting_upsert_values
       ) do
    Ash.Filter.Runtime.filter_matches(domain, records, filter,
      parent: parent,
      tenant: tenant,
      actor: actor,
      conflicting_upsert_values: conflicting_upsert_values
    )
  end

  @doc false
  @impl true
  def upsert(resource, changeset, keys, identity, opts \\ [from_bulk_create?: false]) do
    pkey = Ash.Resource.Info.primary_key(resource)
    keys = keys || pkey

    if (is_nil(identity) || !identity.nils_distinct?) &&
         Enum.any?(keys, &is_nil(Ash.Changeset.get_attribute(changeset, &1))) do
      create(resource, changeset, opts[:from_bulk_create?])
    else
      key_filters =
        Enum.map(keys, fn key ->
          {key,
           Ash.Changeset.get_attribute(changeset, key) || Map.get(changeset.params, key) ||
             Map.get(changeset.params, to_string(key))}
        end)

      query =
        resource
        |> Ash.Query.do_filter(and: [key_filters])
        |> then(fn query ->
          if is_nil(identity) || is_nil(identity.where) do
            query
          else
            Ash.Query.do_filter(query, identity.where)
          end
        end)

      to_set = Ash.Changeset.set_on_upsert(changeset, keys)

      resource
      |> resource_to_query(changeset.domain)
      |> Map.put(:filter, query.filter)
      |> Map.put(:tenant, changeset.tenant)
      |> run_query(resource)
      |> case do
        {:ok, []} ->
          create(resource, changeset, opts[:from_bulk_create?])

        {:ok, [result]} ->
          with {:ok, conflicting_upsert_values} <- Ash.Changeset.apply_attributes(changeset),
               {:ok, [^result]} <-
                 upsert_conflict_check(
                   changeset,
                   result,
                   conflicting_upsert_values
                 ) do
            changeset =
              changeset
              |> Map.put(:attributes, %{})
              |> Map.put(:data, result)
              |> Ash.Changeset.force_change_attributes(to_set)

            update(
              resource,
              %{changeset | action_type: :update, filter: nil},
              Map.take(result, pkey),
              opts[:from_bulk_create?]
            )
          else
            {:ok, []} ->
              {:ok, Ash.Resource.put_metadata(result, :upsert_skipped, true)}

            {:error, reason} ->
              {:error, reason}
          end

        {:ok, _} ->
          {:error, "Multiple records matching keys"}
      end
    end
  end

  @spec upsert_conflict_check(
          changeset :: Ash.Changeset.t(),
          subject :: record,
          conflicting_upsert_values :: record
        ) :: {:ok, [record]} | {:error, reason}
        when record: Ash.Resource.record(), reason: term()
  defp upsert_conflict_check(changeset, subject, conflicting_upsert_values)

  defp upsert_conflict_check(
         %Ash.Changeset{filter: nil},
         result,
         _conflicting_upsert_values
       ),
       do: {:ok, [result]}

  defp upsert_conflict_check(
         %Ash.Changeset{filter: filter, domain: domain, context: context},
         result,
         conflicting_upsert_values
       ) do
    filter_matches(
      [result],
      filter,
      domain,
      context.private[:tenant],
      context.private[:actor],
      nil,
      conflicting_upsert_values
    )
  end

  @impl true
  def bulk_create(resource, stream, options) do
    stream = Enum.to_list(stream)
    log_bulk_create(resource, stream, options)

    if options[:upsert?] do
      # This is not optimized, but thats okay for now
      stream
      |> Enum.reduce_while({:ok, []}, fn changeset, {:ok, results} ->
        changeset =
          Ash.Changeset.set_context(changeset, %{
            private: %{upsert_fields: options[:upsert_fields] || []}
          })

        case upsert(
               resource,
               changeset,
               options.upsert_keys,
               options.identity,
               Map.put(options, :from_bulk_create?, true)
             ) do
          {:ok, result} ->
            if Ash.Resource.get_metadata(result, :upsert_skipped) do
              {:cont, {:ok, results}}
            else
              {:cont,
               {:ok,
                [
                  Ash.Resource.put_metadata(
                    result,
                    :bulk_create_index,
                    changeset.context.bulk_create.index
                  )
                  | results
                ]}}
            end

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
    else
      with {:ok, table} <- wrap_or_create_table(resource, options.tenant) do
        Enum.reduce_while(stream, {:ok, []}, fn changeset, {:ok, results} ->
          pkey =
            resource
            |> Ash.Resource.Info.primary_key()
            |> Enum.into(%{}, fn attr ->
              {attr, Ash.Changeset.get_attribute(changeset, attr)}
            end)

          with {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
               record <- unload_relationships(resource, record) do
            {:cont, {:ok, [{pkey, changeset.context.bulk_create.index, record} | results]}}
          else
            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
        |> case do
          {:ok, records} ->
            case put_or_insert_new_batch(table, records, resource, options.return_records?) do
              :ok ->
                :ok

              {:ok, records} ->
                {:ok, Stream.map(records, &set_loaded/1)}

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end
      end
    end
  end

  @doc false
  @impl true
  def create(resource, changeset, from_bulk_create? \\ false) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ash.Changeset.get_attribute(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         _ <- unless(from_bulk_create?, do: log_create(resource, changeset)),
         {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
         record <- unload_relationships(resource, record),
         {:ok, record} <- put_or_insert_new(table, {pkey, record}, resource) do
      {:ok, set_loaded(record)}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp set_loaded(%resource{} = record) do
    %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}
  end

  defp put_or_insert_new(table, {pkey, record}, resource) do
    attributes = resource |> Ash.Resource.Info.attributes()

    case dump_to_native(record, attributes) do
      {:ok, casted} ->
        case ETS.Set.put(table, {pkey, casted}) do
          {:ok, set} ->
            {_key, record} = ETS.Set.get!(set, pkey)
            cast_record(record, resource)

          other ->
            other
        end

      other ->
        other
    end
  end

  defp put_or_insert_new_batch(table, records, resource, return_records?) do
    attributes = resource |> Ash.Resource.Info.attributes()

    Enum.reduce_while(records, {:ok, [], []}, fn {pkey, index, record}, {:ok, acc, indices} ->
      case dump_to_native(record, attributes) do
        {:ok, casted} ->
          {:cont, {:ok, [{pkey, casted} | acc], [{pkey, index} | indices]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, batch, indices} ->
        case ETS.Set.put(table, batch) do
          {:ok, set} ->
            if return_records? do
              Enum.reduce_while(indices, {:ok, []}, fn {pkey, index}, {:ok, acc} ->
                {_key, record} = ETS.Set.get!(set, pkey)

                case cast_record(record, resource) do
                  {:ok, casted} ->
                    {:cont,
                     {:ok, [Ash.Resource.put_metadata(casted, :bulk_create_index, index) | acc]}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              end)
            else
              :ok
            end

          other ->
            other
        end

      other ->
        other
    end
  end

  @doc false
  def dump_to_native(record, attributes) do
    Enum.reduce_while(attributes, {:ok, %{}}, fn attribute, {:ok, attrs} ->
      case Map.fetch(record, attribute.name) do
        :error ->
          {:cont, {:ok, attrs}}

        {:ok, value} ->
          case Ash.Type.dump_to_native(
                 attribute.type,
                 value,
                 attribute.constraints
               ) do
            {:ok, casted_value} ->
              {:cont, {:ok, Map.put(attrs, attribute.name, casted_value)}}

            :error ->
              {:halt,
               {:error,
                "Failed to dump #{inspect(Map.get(record, attribute.name))} as type #{inspect(attribute.type)}"}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
      end
    end)
  end

  @doc false
  @impl true
  # This is synthesized behavior. Its not truly atomic.
  def destroy_query(query, changeset, resource, options) do
    acc =
      if options[:return_records?] do
        {:ok, []}
      else
        :ok
      end

    log_destroy_query(resource, query)

    query
    |> run_query(resource)
    |> case do
      {:ok, results} ->
        results
        |> Enum.reduce_while(acc, fn result, acc ->
          case destroy(query.resource, %{changeset | data: result}) do
            :ok ->
              case acc do
                :ok ->
                  {:cont, :ok}

                {:ok, results} ->
                  {:cont, {:ok, [result | results]}}
              end

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
        |> case do
          :ok -> :ok
          {:ok, results} -> {:ok, Enum.reverse(results)}
          {:error, error} -> {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  @impl true
  def destroy(resource, %{data: record, filter: filter} = changeset) do
    do_destroy(
      resource,
      record,
      changeset.tenant,
      filter,
      changeset.domain,
      changeset.context[:private][:actor]
    )
  end

  defp do_destroy(resource, record, tenant, filter, domain, actor) do
    with {:ok, table} <- wrap_or_create_table(resource, tenant) do
      pkey = Map.take(record, Ash.Resource.Info.primary_key(resource))

      if has_filter?(filter) do
        case ETS.Set.get(table, pkey) do
          {:ok, {_key, record}} when is_map(record) ->
            with {:ok, record} <- cast_record(record, resource),
                 {:ok, [_]} <- filter_matches([record], filter, domain, tenant, actor) do
              with {:ok, _} <- ETS.Set.delete(table, pkey) do
                :ok
              end
            else
              {:ok, []} ->
                {:error,
                 Ash.Error.Changes.StaleRecord.exception(
                   resource: resource,
                   filter: filter
                 )}

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end
      else
        with {:ok, _} <- ETS.Set.delete(table, pkey) do
          :ok
        end
      end
    end
  end

  defp has_filter?(filter) when filter in [nil, true], do: false
  defp has_filter?(%Ash.Filter{expression: expression}) when expression == true, do: false
  defp has_filter?(_filter), do: true

  @doc false
  @impl true
  # This is synthesized behavior. Its not truly atomic.
  def update_query(query, changeset, resource, options) do
    acc =
      if options[:return_records?] do
        {:ok, []}
      else
        :ok
      end

    log_update_query(resource, query, changeset)

    query
    |> Map.update!(:filter, fn filter ->
      if is_nil(changeset.filter) do
        filter
      else
        filter = filter || %Ash.Filter{resource: changeset.resource}
        Ash.Filter.add_to_filter!(filter, changeset.filter)
      end
    end)
    |> run_query(resource)
    |> case do
      {:ok, results} ->
        Enum.reduce_while(results, acc, fn result, acc ->
          case update(query.resource, %{changeset | data: result}, nil, true) do
            {:ok, result} ->
              case acc do
                :ok ->
                  {:cont, :ok}

                {:ok, results} ->
                  {:cont, {:ok, [result | results]}}
              end

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)

      {:error, error} ->
        {:error, error}
    end
    |> case do
      :ok -> :ok
      {:ok, results} -> {:ok, Enum.reverse(results)}
      {:error, error} -> {:error, error}
    end
  end

  @doc false
  @impl true
  def update(resource, changeset, pkey \\ nil, from_bulk? \\ false) do
    pkey = pkey || pkey_map(resource, changeset.data)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         _ <- unless(from_bulk?, do: log_update(resource, pkey, changeset)),
         {:ok, record} <-
           do_update(
             table,
             {pkey, changeset.attributes, changeset.atomics, changeset.filter},
             changeset.domain,
             changeset.tenant,
             resource,
             changeset.context[:private][:actor]
           ),
         {:ok, record} <- cast_record(record, resource) do
      new_pkey = pkey_map(resource, record)

      if new_pkey != pkey do
        case destroy(resource, changeset) do
          :ok ->
            {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

          {:error, error} ->
            {:error, error}
        end
      else
        {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}
      end
    else
      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def calculate(resource, expressions, context) do
    Enum.reduce_while(expressions, {:ok, []}, fn expression, {:ok, results} ->
      case Ash.Expr.eval(expression, resource: resource, context: context) do
        {:ok, result} -> {:cont, {:ok, [result | results]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, results} -> {:ok, Enum.reverse(results)}
      {:error, error} -> {:error, error}
    end
  end

  @doc false
  def pkey_map(resource, data) do
    resource
    |> Ash.Resource.Info.primary_key()
    |> Enum.into(%{}, fn attr ->
      {attr, Map.get(data, attr)}
    end)
  end

  defp do_update(
         table,
         {pkey, record, atomics, changeset_filter},
         domain,
         tenant,
         resource,
         actor
       ) do
    attributes = resource |> Ash.Resource.Info.attributes()

    case dump_to_native(record, attributes) do
      {:ok, casted} ->
        case ETS.Set.get(table, pkey) do
          {:ok, {_key, record}} when is_map(record) ->
            with {:ok, casted_record} <- cast_record(record, resource),
                 {:ok, [casted_record]} <-
                   filter_matches([casted_record], changeset_filter, domain, tenant, actor) do
              case atomics do
                empty when empty in [nil, []] ->
                  data = Map.merge(record, casted)

                  put_data(table, pkey, data)

                atomics ->
                  with {:ok, atomics} <- make_atomics(atomics, resource, domain, casted_record) do
                    data = record |> Map.merge(casted) |> Map.merge(atomics)
                    put_data(table, pkey, data)
                  end
              end
            else
              {:error, error} ->
                {:error, error}

              {:ok, []} ->
                {:error,
                 Ash.Error.Changes.StaleRecord.exception(
                   resource: resource,
                   filter: changeset_filter
                 )}
            end

          {:ok, _} ->
            {:error,
             Ash.Error.Changes.StaleRecord.exception(
               resource: record.__struct__,
               filter: changeset_filter
             )}

          other ->
            other
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp put_data(table, pkey, data) do
    case ETS.Set.put(
           table,
           {pkey, data}
         ) do
      {:ok, _set} ->
        {:ok, data}

      error ->
        error
    end
  end

  defp make_atomics(atomics, resource, domain, record) do
    Enum.reduce_while(atomics, {:ok, %{}}, fn {key, expr}, {:ok, acc} ->
      case Ash.Expr.eval(expr,
             resource: resource,
             record: record,
             domain: domain,
             unknown_on_unknown_refs?: true
           ) do
        {:ok, value} ->
          {:cont, {:ok, Map.put(acc, key, value)}}

        {:error, error} ->
          {:halt, {:error, error}}

        :unknown ->
          {:halt, {:error, "Could not evaluate expression #{inspect(expr)}"}}
      end
    end)
  end

  defp unload_relationships(resource, record) do
    empty = resource.__struct__()

    resource
    |> Ash.Resource.Info.relationships()
    |> Enum.reduce(record, fn relationship, record ->
      Map.put(record, relationship.name, Map.get(empty, relationship.name))
    end)
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp wrap_or_create_table(resource, tenant) do
    tenant =
      if Ash.Resource.Info.multitenancy_strategy(resource) == :context do
        tenant
      end

    if Ash.DataLayer.Ets.Info.private?(resource) do
      configured_table = Ash.DataLayer.Ets.Info.table(resource)

      case Process.get({:ash_ets_table, configured_table, tenant}) do
        nil ->
          case ETS.Set.new(
                 protection: :private,
                 ordered: true,
                 read_concurrency: true
               ) do
            {:ok, table} ->
              Process.put({:ash_ets_table, configured_table, tenant}, table)
              {:ok, table}

            {:error, error} ->
              {:error, error}
          end

        tab ->
          {:ok, tab}
      end
    else
      TableManager.start(resource, tenant)
    end
  end

  defp log_bulk_create(resource, stream, options) do
    Logger.debug(
      "#{bulk_create_operation(options, stream)} #{inspect(resource)}: #{inspect(stream)}"
    )
  end

  defp bulk_create_operation(
         %{
           upsert?: true,
           upsert_keys: upsert_keys,
           upsert_fields: upsert_fields,
           upsert_where: expr
         },
         stream
       ) do
    where_expr =
      if is_nil(expr) do
        ""
      else
        "where #{inspect(expr)}"
      end

    "Upserting #{Enum.count(stream)} on #{inspect(upsert_keys)} #{where_expr}, setting #{inspect(List.wrap(upsert_fields))}"
  end

  defp bulk_create_operation(_options, stream) do
    "Creating #{Enum.count(stream)}"
  end

  defp log_destroy_query(resource, query) do
    limit =
      if query.limit do
        "#{query.limit} "
      else
        ""
      end

    offset =
      if query.offset && query.offset != 0 do
        " skipping #{query.offset} records"
      else
        ""
      end

    sort =
      if query.sort && query.sort != [] do
        " sorted by #{inspect(query.sort)}"
      else
        ""
      end

    filter =
      if query.filter && query.filter != nil && query.filter.expression != nil do
        " where `#{inspect(query.filter.expression)}`"
      else
        ""
      end

    Logger.debug("""
    ETS: Destroying #{limit}#{inspect(resource)}#{offset}#{sort}#{filter}
    """)

    :ok
  end

  defp log_update_query(resource, query, changeset) do
    limit =
      if query.limit do
        "#{query.limit} "
      else
        ""
      end

    offset =
      if query.offset && query.offset != 0 do
        " skipping #{query.offset} records"
      else
        ""
      end

    sort =
      if query.sort && query.sort != [] do
        " sorted by #{inspect(query.sort)}"
      else
        ""
      end

    filter =
      if query.filter && query.filter != nil && query.filter.expression != nil do
        " matching filter `#{inspect(query.filter.expression)}`"
      else
        ""
      end

    Logger.debug("""
    ETS: Updating #{limit}#{inspect(resource)}#{offset}#{sort}#{filter}:

    #{inspect(Map.merge(changeset.attributes, Map.new(changeset.atomics)), pretty: true)}
    """)

    :ok
  end

  defp log_create(resource, changeset) do
    Logger.debug("""
    Creating #{inspect(resource)}:

    #{inspect(Map.merge(changeset.attributes, Map.new(changeset.atomics)), pretty: true)}
    """)
  end

  defp log_update(resource, pkey, changeset) do
    pkey =
      if Enum.count_until(pkey, 2) == 2 do
        inspect(pkey)
      else
        inspect(pkey |> Enum.at(0) |> elem(1))
      end

    Logger.debug("""
    "Updating #{inspect(resource)} #{pkey}:

    #{inspect(Map.merge(changeset.attributes, Map.new(changeset.atomics)), pretty: true)}
    """)
  end
end
