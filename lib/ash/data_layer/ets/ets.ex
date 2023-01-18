defmodule Ash.DataLayer.Ets do
  @behaviour Ash.DataLayer

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

  <!--- ash-hq-hide-start--> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index([@ets])}

  ### Docs

  #{Spark.Dsl.Extension.doc([@ets])}
  <!--- ash-hq-hide-stop--> <!--- -->
  """

  use Spark.Dsl.Extension,
    sections: [@ets],
    transformers: [Ash.DataLayer.Transformers.RequirePreCheckWith]

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
        if tenant do
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
    if Ash.DataLayer.Ets.Info.private?(resource) do
      case Process.get({:ash_ets_table, resource, tenant}) do
        nil ->
          :ok

        table ->
          ETS.Set.delete(table)
      end
    else
      table =
        if tenant do
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
  def can?(resource, :async_engine) do
    not private?(resource)
  end

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
  def can?(_, :create), do: true
  def can?(_, :read), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :filter), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :transact), do: false
  def can?(_, {:filter_expr, _}), do: true

  def can?(resource, {:join, other_resource}) do
    # See the comment in can?/2 in mnesia data layer to explain this
    not (private?(resource) and
           Ash.DataLayer.data_layer(other_resource) == Ash.DataLayer.Mnesia)
  end

  def can?(_, :nested_expressions), do: true
  def can?(_, {:query_aggregate, :count}), do: true
  def can?(_, {:sort, _}), do: true
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
  def run_aggregate_query(%{api: api} = query, aggregates, resource) do
    case run_query(query, resource) do
      {:ok, results} ->
        Enum.reduce_while(aggregates, {:ok, %{}}, fn
          %{kind: :count, name: name, query: query}, {:ok, acc} ->
            results
            |> filter_matches(Map.get(query || %{}, :filter), api)
            |> case do
              {:ok, matches} ->
                {:cont, {:ok, Map.put(acc, name, Enum.count(matches))}}

              {:error, error} ->
                {:halt, {:error, error}}
            end

          _, _ ->
            {:halt, {:error, "unsupported aggregate"}}
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
          tenant: tenant,
          calculations: calculations,
          aggregates: aggregates,
          api: api
        },
        _resource
      ) do
    with {:ok, records} <- get_records(resource, tenant),
         {:ok, records} <- do_add_aggregates(records, api, resource, aggregates),
         {:ok, records} <-
           filter_matches(records, filter, api),
         {:ok, records} <-
           do_add_calculations(records, resource, calculations) do
      offset_records =
        records
        |> Sort.runtime_sort(sort)
        |> Enum.drop(offset || 0)

      if limit do
        {:ok, Enum.take(offset_records, limit)}
      else
        {:ok, offset_records}
      end
    else
      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp do_add_calculations(records, _resource, []), do: {:ok, records}

  defp do_add_calculations(records, resource, calculations) do
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      calculations
      |> Enum.reduce_while({:ok, record}, fn calculation, {:ok, record} ->
        expression = calculation.module.expression(calculation.opts, calculation.context)

        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               public?: false
             }) do
          {:ok, expression} ->
            case Ash.Expr.eval_hydrated(expression, record: record) do
              {:ok, value} ->
                if calculation.load do
                  {:cont, {:ok, Map.put(record, calculation.load, value)}}
                else
                  {:cont,
                   {:ok,
                    Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))}}
                end

              :unknown ->
                if calculation.load do
                  {:cont, {:ok, Map.put(record, calculation.load, nil)}}
                else
                  {:cont,
                   {:ok, Map.update!(record, :calculations, &Map.put(&1, calculation.name, nil))}}
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

  defp do_add_aggregates(records, _api, _resource, []), do: {:ok, records}

  defp do_add_aggregates(records, api, _resource, aggregates) do
    # TODO support crossing apis by getting the destination api, and set destination query context.
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, records} ->
      aggregates
      |> Enum.reduce_while(
        {:ok, record},
        fn
          %{
            kind: :count,
            relationship_path: relationship_path,
            query: query,
            authorization_filter: authorization_filter,
            name: name,
            load: load
          },
          {:ok, record} ->
            query =
              if authorization_filter do
                Ash.Query.do_filter(query, authorization_filter)
              else
                query
              end

            with {:ok, loaded_record} <- api.load(record, relationship_path),
                 related <- Ash.Filter.Runtime.get_related(loaded_record, relationship_path),
                 {:ok, filtered} <-
                   filter_matches(related, query.filter, api) do
              {:cont, {:ok, Map.put(record, load || name, Enum.count(filtered))}}
            else
              other ->
                {:halt, other}
            end

          %{
            kind: :first,
            relationship_path: relationship_path,
            query: query,
            authorization_filter: authorization_filter,
            name: name,
            load: load,
            field: field
          },
          {:ok, record} ->
            query =
              if authorization_filter do
                Ash.Query.do_filter(query, authorization_filter)
              else
                query
              end

            with {:ok, loaded_record} <- api.load(record, relationship_path),
                 related <- Ash.Filter.Runtime.get_related(loaded_record, relationship_path),
                 {:ok, filtered} <-
                   filter_matches(related, query.filter, api) do
              value =
                case filtered do
                  [first | _] ->
                    Map.get(first, field)

                  _ ->
                    nil
                end

              {:cont, {:ok, Map.put(record, load || name, value)}}
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

  defp filter_matches(records, nil, _api), do: {:ok, records}

  defp filter_matches(records, filter, api) do
    Ash.Filter.Runtime.filter_matches(api, records, filter)
  end

  @doc false
  @impl true
  def upsert(resource, changeset, keys) do
    keys = keys || Ash.Resource.Info.primary_key(resource)

    if Enum.any?(keys, &is_nil(Ash.Changeset.get_attribute(changeset, &1))) do
      create(resource, changeset)
    else
      key_filters =
        Enum.map(keys, fn key ->
          {key, Ash.Changeset.get_attribute(changeset, key)}
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

          update(resource, changeset)

        {:ok, _} ->
          {:error, "Multiple records matching keys"}
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
      {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}
    else
      {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
    end
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

  @doc false
  def dump_to_native(record, attributes) do
    Enum.reduce_while(attributes, {:ok, %{}}, fn attribute, {:ok, attrs} ->
      case Map.get(record, attribute.name) do
        nil ->
          {:cont, {:ok, Map.put(attrs, attribute.name, nil)}}

        value ->
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
  def update(resource, changeset) do
    pkey = pkey_map(resource, changeset.data)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
         {:ok, record} <-
           do_update(table, {pkey, record}, resource),
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
            case ETS.Set.put(table, {pkey, Map.merge(record, casted)}) do
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
