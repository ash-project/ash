defmodule Ash.DataLayer.Ets do
  @behaviour Ash.DataLayer
  require Ash.Query

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

  @deprecated "use Ash.DataLayer.Ets.Info.private?/1 instead"
  defdelegate private?(resource), to: Ash.DataLayer.Ets.Info

  @deprecated "use Ash.DataLayer.Ets.Info.table/1 instead"
  defdelegate table(resource), to: Ash.DataLayer.Ets.Info

  defmodule Query do
    @moduledoc false
    defstruct [
      :resource,
      :filter,
      :limit,
      :sort,
      :tenant,
      :api,
      :distinct,
      :distinct_sort,
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
  def can?(resource, :async_engine), do: not private?(resource)
  def can?(_, {:lateral_join, _}), do: true
  def can?(_, :bulk_create), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :expression_calculation), do: true
  def can?(_, :expression_calculation_sort), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, :upsert), do: true
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

  def can?(resource, {:join, other_resource}) do
    # See the comment in can?/2 in mnesia data layer to explain this
    not (private?(resource) and
           Ash.DataLayer.data_layer(other_resource) == Ash.DataLayer.Mnesia)
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
  def can?(_, {:sort, _}), do: true
  def can?(_, {:atomic, :update}), do: true
  def can?(_, {:atomic, :upsert}), do: true
  def can?(_, _), do: false

  @doc false
  @impl true
  def resource_to_query(resource, api) do
    %Query{
      resource: resource,
      api: api
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
  def add_calculation(query, calculation, _, _),
    do: {:ok, %{query | calculations: [calculation | query.calculations]}}

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
  def run_aggregate_query(%{api: api} = query, aggregates, resource) do
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
            default_value: default_value
          },
          {:ok, acc} ->
            results
            |> filter_matches(Map.get(query || %{}, :filter), api)
            |> case do
              {:ok, matches} ->
                field = field || Enum.at(Ash.Resource.Info.primary_key(resource), 0)

                value = aggregate_value(matches, kind, field, uniq?, default_value)
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
        {:error, Ash.Error.to_ash_error(error)}

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
          api: api
        },
        _resource,
        parent \\ nil
      ) do
    with {:ok, records} <- get_records(resource, tenant),
         {:ok, records} <-
           filter_matches(records, filter, api, parent),
         records <- Sort.runtime_sort(records, distinct_sort || sort, api: api),
         records <- Sort.runtime_distinct(records, distinct, api: api),
         records <- Sort.runtime_sort(records, sort, api: api),
         records <- Enum.drop(records, offset || []),
         records <- do_limit(records, limit),
         {:ok, records} <- do_add_aggregates(records, api, resource, aggregates),
         {:ok, records} <-
           do_add_calculations(records, resource, calculations, api) do
      {:ok, records}
    else
      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
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
        [{source_query, source_attribute, destination_attribute, relationship}]
      ) do
    source_attributes = Enum.map(root_data, &Map.get(&1, source_attribute))

    source_query
    |> Ash.Query.filter(ref(^source_attribute) in ^source_attributes)
    |> Ash.Query.set_context(%{private: %{internal?: true}})
    |> Ash.Query.unset(:load)
    |> Ash.Query.unset(:select)
    |> query.api.read(authorize?: false)
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
              Ash.Filter.add_to_filter(query.filter, [
                {destination_attribute, Map.get(parent, source_attribute)}
              ])
            end

          query = %{query | filter: new_filter}

          case run_query(query, relationship.source, parent) do
            {:ok, new_results} ->
              new_results =
                Enum.map(
                  new_results,
                  &Map.put(&1, :__lateral_join_source__, Map.get(parent, source_attribute))
                )

              {:cont, {:ok, new_results ++ results}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
        |> case do
          {:ok, results} ->
            {:ok, results}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  def run_query_with_lateral_join(query, root_data, _destination_resource, [
        {source_query, source_attribute, source_attribute_on_join_resource, relationship},
        {through_query, destination_attribute_on_join_resource, destination_attribute,
         _through_relationship}
      ]) do
    source_attributes = Enum.map(root_data, &Map.get(&1, source_attribute))

    source_query
    |> Ash.Query.unset(:load)
    |> Ash.Query.filter(ref(^source_attribute) in ^source_attributes)
    |> Ash.Query.set_context(%{private: %{internal?: true}})
    |> query.api.read(authorize?: false)
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, root_data} ->
        root_data
        |> Enum.reduce_while({:ok, []}, fn parent, {:ok, results} ->
          through_query
          |> Ash.Query.filter(
            ref(^source_attribute_on_join_resource) ==
              ^Map.get(parent, source_attribute)
          )
          |> Ash.Query.set_context(%{private: %{internal?: true}})
          |> query.api.read(authorize?: false)
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
                  Ash.Filter.add_to_filter(query.filter, [
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
                        if Map.get(join_row, destination_attribute_on_join_resource) ==
                             Map.get(result, destination_attribute) do
                          [
                            Map.put(
                              result,
                              :__lateral_join_source__,
                              Map.get(join_row, source_attribute_on_join_resource)
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
        |> case do
          {:ok, results} ->
            {:ok, results}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  def do_add_calculations(records, _resource, [], _api), do: {:ok, records}

  def do_add_calculations(records, resource, calculations, api) do
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      calculations
      |> Enum.reduce_while({:ok, record}, fn calculation, {:ok, record} ->
        expression = calculation.module.expression(calculation.opts, calculation.context)

        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               public?: false
             }) do
          {:ok, expression} ->
            case Ash.Expr.eval_hydrated(expression, record: record, resource: resource, api: api) do
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
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  @doc false
  def do_add_aggregates(records, _api, _resource, []), do: {:ok, records}

  def do_add_aggregates(records, api, _resource, aggregates) do
    # TODO support crossing apis by getting the destination api, and set destination query context.
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
            default_value: default_value
          },
          {:ok, record} ->
            with {:ok, loaded_record} <-
                   api.load(record, relationship_path_to_load(relationship_path, field)),
                 related <-
                   Ash.Filter.Runtime.get_related(loaded_record, relationship_path),
                 {:ok, filtered} <-
                   filter_matches(related, query.filter, api),
                 sorted <-
                   Sort.runtime_sort(filtered, query.sort, api: api) do
              field = field || Enum.at(Ash.Resource.Info.primary_key(query.resource), 0)

              value =
                aggregate_value(sorted, kind, field, uniq?, default_value)

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
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp relationship_path_to_load([], leaf) do
    leaf
  end

  defp relationship_path_to_load([key | rest], leaf) do
    [{key, relationship_path_to_load(rest, leaf)}]
  end

  @doc false
  def aggregate_value(records, kind, field, uniq?, default) do
    case kind do
      :count ->
        if uniq? do
          records
          |> Stream.map(&Map.get(&1, field))
          |> Stream.uniq()
          |> Stream.reject(&is_nil/1)
          |> Enum.count()
        else
          Enum.count(records, &(not is_nil(Map.get(&1, field))))
        end

      :exists ->
        case records do
          [] ->
            false

          _ ->
            true
        end

      :first ->
        case records do
          [] ->
            default

          [record | _rest] ->
            Map.get(record, field)
        end

      :list ->
        records
        |> Enum.map(fn record ->
          Map.get(record, field)
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
            |> Stream.map(&Map.get(&1, field))
            |> Stream.uniq()
          else
            records
            |> Stream.map(&Map.get(&1, field))
            |> Stream.uniq()
          end
        end)
        |> Enum.reduce({0, 0}, fn value, {sum, count} ->
          case value do
            nil ->
              {sum, count}

            value ->
              {sum + (value || 0), count + 1}
          end
        end)
        |> case do
          {_, 0} ->
            nil

          {sum, count} ->
            sum / count
        end

      kind when kind in [:sum, :max, :min] ->
        records
        |> Enum.map(&Map.get(&1, field))
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

            case kind do
              :sum ->
                Enum.sum(items)

              :max ->
                Enum.max(items)

              :min ->
                Enum.min(items)
            end
        end
    end
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

  defp filter_matches(records, filter, api, parent \\ nil)
  defp filter_matches(records, nil, _api, _parent), do: {:ok, records}

  defp filter_matches(records, filter, api, parent) do
    Ash.Filter.Runtime.filter_matches(api, records, filter, parent: parent)
  end

  @doc false
  @impl true
  def upsert(resource, changeset, keys) do
    pkey = Ash.Resource.Info.primary_key(resource)
    keys = keys || pkey

    if Enum.any?(keys, &is_nil(Ash.Changeset.get_attribute(changeset, &1))) do
      create(resource, changeset)
    else
      key_filters =
        Enum.map(keys, fn key ->
          {key,
           Ash.Changeset.get_attribute(changeset, key) || Map.get(changeset.params, key) ||
             Map.get(changeset.params, to_string(key))}
        end)

      query = Ash.Query.do_filter(resource, and: [key_filters])

      resource
      |> resource_to_query(changeset.api)
      |> Map.put(:filter, query.filter)
      |> Map.put(:tenant, changeset.tenant)
      |> run_query(resource)
      |> case do
        {:ok, []} ->
          create(resource, changeset)

        {:ok, [result]} ->
          to_set = Ash.Changeset.set_on_upsert(changeset, keys)

          changeset =
            changeset
            |> Map.put(:attributes, %{})
            |> Map.put(:data, result)
            |> Ash.Changeset.force_change_attributes(to_set)

          update(resource, changeset, Map.take(result, pkey))

        {:ok, _} ->
          {:error, "Multiple records matching keys"}
      end
    end
  end

  @impl true
  def bulk_create(resource, stream, options) do
    if options[:upsert?] do
      # This is not optimized, but thats okay for now
      stream
      |> Enum.reduce_while({:ok, []}, fn changeset, {:ok, results} ->
        changeset =
          Ash.Changeset.set_context(changeset, %{
            private: %{upsert_fields: options[:upsert_fields] || {:replace, []}}
          })

        case upsert(resource, changeset, options.upsert_keys) do
          {:ok, result} ->
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
  def create(resource, changeset) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ash.Changeset.get_attribute(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
         record <- unload_relationships(resource, record),
         {:ok, record} <-
           put_or_insert_new(table, {pkey, record}, resource) do
      {:ok, set_loaded(record)}
    else
      {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
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
  def destroy(resource, %{data: record} = changeset) do
    do_destroy(resource, record, changeset.tenant)
  end

  defp do_destroy(resource, record, tenant) do
    pkey = Map.take(record, Ash.Resource.Info.primary_key(resource))

    with {:ok, table} <- wrap_or_create_table(resource, tenant),
         {:ok, _} <- ETS.Set.delete(table, pkey) do
      :ok
    else
      {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
    end
  end

  @doc false
  @impl true
  def update(resource, changeset, pkey \\ nil) do
    pkey = pkey || pkey_map(resource, changeset.data)

    atomic_changes =
      Enum.reduce_while(changeset.atomics, {:ok, %{}}, fn {key, value}, {:ok, acc} ->
        case Ash.Expr.eval(value, resource: resource, record: changeset.data) do
          {:ok, value} ->
            {:cont, {:ok, Map.put(acc, key, value)}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)

    with {:ok, atomics} <- atomic_changes,
         {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         {:ok, record} <-
           do_update(table, {pkey, Map.merge(changeset.attributes, atomics)}, resource),
         {:ok, record} <- cast_record(record, resource) do
      new_pkey = pkey_map(resource, record)

      if new_pkey != pkey do
        case destroy(resource, changeset) do
          :ok ->
            {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

          {:error, error} ->
            {:error, Ash.Error.to_ash_error(error)}
        end
      else
        {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}
      end
    else
      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
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

  defp do_update(table, {pkey, record}, resource) do
    attributes = resource |> Ash.Resource.Info.attributes()

    case dump_to_native(record, attributes) do
      {:ok, casted} ->
        case ETS.Set.get(table, pkey) do
          {:ok, {_key, record}} when is_map(record) ->
            case ETS.Set.put(
                   table,
                   {pkey, Map.merge(record, casted)}
                 ) do
              {:ok, set} ->
                {_key, record} = ETS.Set.get!(set, pkey)
                {:ok, record}

              error ->
                error
            end

          {:ok, _} ->
            {:error, "Record not found matching: #{inspect(pkey)}"}

          other ->
            other
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp unload_relationships(resource, record) do
    empty = resource.__struct__

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
end
