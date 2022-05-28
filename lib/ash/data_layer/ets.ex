defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing.

  This is used for testing. *Do not use this data layer in production*
  """

  @behaviour Ash.DataLayer

  @ets %Ash.Dsl.Section{
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
        default: false
      ]
    ]
  }

  use Ash.Dsl.Extension,
    sections: [@ets],
    transformers: [Ash.DataLayer.Transformers.RequirePreCheckWith]

  alias Ash.Actions.Sort
  alias Ash.Dsl.Extension

  @spec private?(Ash.Resource.t()) :: boolean
  def private?(resource) do
    Extension.get_opt(resource, [:ets], :private?, false, true)
  end

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
          Module.concat(to_string(resource), to_string(tenant))
        else
          resource
        end

      if Ash.DataLayer.Ets.private?(resource) do
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

    defp do_wrap_existing(resource, table) do
      case ETS.Set.wrap_existing(table) do
        {:error, :table_not_found} ->
          protection =
            if Ash.DataLayer.Ets.private?(resource) do
              :private
            else
              :public
            end

          case ETS.Set.new(
                 name: table,
                 protection: protection,
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

  @impl true
  def can?(resource, :async_engine) do
    not private?(resource)
  end

  def can?(_, :composite_primary_key), do: true
  def can?(_, :expression_calculation), do: true
  def can?(_, :expression_calculation_sort), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, :upsert), do: true
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

  @impl true
  def resource_to_query(resource, api) do
    %Query{
      resource: resource,
      api: api
    }
  end

  @impl true
  def limit(query, offset, _), do: {:ok, %{query | limit: offset}}

  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

  @impl true
  def add_calculation(query, calculation, _, _),
    do: {:ok, %{query | calculations: [calculation | query.calculations]}}

  @impl true
  def set_tenant(_resource, query, tenant) do
    {:ok, %{query | tenant: tenant}}
  end

  @impl true
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

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
  end

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
          api: api
        },
        _resource
      ) do
    with {:ok, records} <- get_records(resource, tenant),
         {:ok, filtered_records} <- filter_matches(records, filter, api) do
      offset_records =
        filtered_records
        |> Sort.runtime_sort(sort)
        |> Enum.drop(offset || 0)

      limited_records =
        if limit do
          Enum.take(offset_records, limit)
        else
          offset_records
        end

      if Enum.empty?(calculations) do
        {:ok, limited_records}
      else
        Enum.reduce_while(limited_records, {:ok, []}, fn record, {:ok, records} ->
          calculations
          |> Enum.reduce_while({:ok, record}, fn calculation, {:ok, record} ->
            expression = calculation.module.expression(calculation.opts, calculation.context)

            case Ash.Filter.hydrate_refs(expression, %{
                   resource: resource,
                   aggregates: %{},
                   calculations: %{},
                   public?: false
                 }) do
              {:ok, expression} ->
                case Ash.Filter.Runtime.do_match(record, expression) do
                  {:ok, value} ->
                    if calculation.load do
                      {:cont, {:ok, Map.put(record, calculation.load, value)}}
                    else
                      {:cont,
                       {:ok,
                        Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))}}
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
          {:ok, records} -> {:ok, Enum.reverse(records)}
          {:error, error} -> {:error, error}
        end
      end
    else
      {:error, error} -> {:error, error}
    end
  end

  defp get_records(resource, tenant) do
    with {:ok, table} <- wrap_or_create_table(resource, tenant),
         {:ok, record_tuples} <- ETS.Set.to_list(table) do
      {:ok, Enum.map(record_tuples, &elem(&1, 1))}
    end
  end

  defp filter_matches(records, nil, _api), do: {:ok, records}

  defp filter_matches(records, filter, api) do
    Ash.Filter.Runtime.filter_matches(api, records, filter)
  end

  @impl true
  def upsert(resource, changeset, keys) do
    if Enum.sort(keys) == Enum.sort(Ash.Resource.Info.primary_key(resource)) do
      create(resource, changeset, upsert?: true)
    else
      key_filters =
        Enum.map(keys, fn key ->
          {key, Ash.Changeset.get_attribute(changeset, key)}
        end)

      if Enum.any?(key_filters, fn {_, val} -> is_nil(val) end) do
        update(resource, changeset)
      else
        query = Ash.Query.do_filter(resource, and: [key_filters])

        resource
        |> resource_to_query(changeset.api)
        |> Map.put(:filter, query.filter)
        |> Map.put(:tenant, changeset.tenant)
        |> run_query(resource)
        |> case do
          {:ok, []} ->
            update(resource, changeset)

          {:ok, results} ->
            Enum.reduce_while(results, :ok, fn result, :ok ->
              case do_destroy(changeset.resource, result, changeset.tenant) do
                :ok ->
                  {:cont, :ok}

                {:error, error} ->
                  {:halt, {:error, error}}
              end
            end)
            |> case do
              :ok ->
                update(resource, changeset)

              {:error, error} ->
                {:error, error}
            end
        end
      end
    end
  end

  @impl true
  def create(resource, changeset, opts \\ []) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ash.Changeset.get_attribute(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
         record <- unload_relationships(resource, record),
         {:ok, _} <-
           put_or_insert_new(table, {pkey, record}, opts) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp put_or_insert_new(table, {pkey, record}, opts) do
    if opts[:upsert?] do
      ETS.Set.put(table, {pkey, record})
    else
      ETS.Set.put_new(table, {pkey, record})
    end
  end

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
      {:error, error} -> {:error, error}
    end
  end

  @impl true
  def update(resource, changeset) do
    create(resource, changeset, upsert?: true)
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
    TableManager.start(resource, tenant)
  end
end
