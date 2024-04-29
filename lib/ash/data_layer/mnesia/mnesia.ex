defmodule Ash.DataLayer.Mnesia do
  @behaviour Ash.DataLayer

  @mnesia %Spark.Dsl.Section{
    name: :mnesia,
    describe: """
    A section for configuring the mnesia data layer
    """,
    examples: [
      """
      mnesia do
        table :custom_table
      end
      """
    ],
    schema: [
      table: [
        type: :atom,
        doc: "The table name to use, defaults to the name of the resource"
      ]
    ]
  }

  @moduledoc """
  An Mnesia backed Ash Datalayer.

  In your application initialization, you will need to call `Mnesia.create_schema([node()])`.

  Additionally, you will want to create your mnesia tables there.

  This data layer is *unoptimized*, fetching all records from a table and filtering them
  in memory. For that reason, it is not recommended to use it with large amounts of data. It can be
  great for prototyping or light usage, though.
  """

  use Spark.Dsl.Extension,
    sections: [@mnesia],
    verifiers: [Ash.DataLayer.Verifiers.RequirePreCheckWith]

  alias Ash.Actions.Sort
  alias :mnesia, as: Mnesia

  @doc """
  Creates the table for each mnesia resource in a domain
  """
  def start(domain, resources \\ []) do
    Mnesia.create_schema([node()])
    Mnesia.start()

    Code.ensure_compiled(domain)

    domain
    |> Ash.Domain.Info.resources()
    |> Enum.concat(resources)
    |> Enum.filter(&(__MODULE__ in Spark.extensions(&1)))
    |> Enum.flat_map(fn resource ->
      resource
      |> Ash.DataLayer.Mnesia.Info.table()
      |> List.wrap()
    end)
    |> Enum.each(&Mnesia.create_table(&1, attributes: [:_pkey, :val]))
  end

  defmodule Query do
    @moduledoc false
    defstruct [
      :domain,
      :resource,
      :filter,
      :limit,
      :sort,
      relationships: %{},
      offset: 0,
      aggregates: [],
      calculations: []
    ]
  end

  @doc false
  @impl true
  def can?(_, :async_engine), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :upsert), do: true
  def can?(_, :create), do: true
  def can?(_, :read), do: true
  def can?(_, :calculate), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :filter), do: true
  def can?(_, {:filter_relationship, _}), do: true
  def can?(_, {:query_aggregate, :count}), do: true
  def can?(_, :expression_calculation), do: true
  def can?(_, :expression_calculation_sort), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :transact), do: true
  def can?(_, :aggregate_filter), do: true
  def can?(_, :aggregate_sort), do: true
  def can?(_, :expr_error), do: true
  def can?(_, {:aggregate_relationship, _}), do: true
  def can?(_, {:aggregate, :count}), do: true
  def can?(_, {:aggregate, :first}), do: true
  def can?(_, {:aggregate, :sum}), do: true
  def can?(_, {:aggregate, :list}), do: true
  def can?(_, {:aggregate, :max}), do: true
  def can?(_, {:aggregate, :min}), do: true
  def can?(_, {:aggregate, :avg}), do: true
  def can?(_, {:aggregate, :exists}), do: true
  def can?(resource, {:query_aggregate, kind}), do: can?(resource, {:aggregate, kind})

  def can?(_, {:join, resource}) do
    # This is to ensure that these can't join, which is necessary for testing
    # if someone needs to use these both and *actually* needs real joins for private
    # ets resources then we can talk about making this only happen in ash tests
    not (Ash.DataLayer.data_layer(resource) == Ash.DataLayer.Ets &&
           Ash.DataLayer.Ets.Info.private?(resource))
  end

  def can?(_, {:filter_expr, _}), do: true
  def can?(_, :nested_expressions), do: true
  def can?(_, {:sort, _}), do: true

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
  def in_transaction?(_), do: Mnesia.is_transaction()

  @doc false
  @impl true
  def limit(query, offset, _), do: {:ok, %{query | limit: offset}}

  @doc false
  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

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
  def add_aggregate(query, aggregate, _),
    do: {:ok, %{query | aggregates: [aggregate | query.aggregates]}}

  @doc false
  @impl true
  def add_calculations(query, calculations, _),
    do: {:ok, %{query | calculations: query.calculations ++ calculations}}

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
            default_value: default_value
          },
          {:ok, acc} ->
            results
            |> filter_matches(Map.get(query || %{}, :filter), domain)
            |> case do
              {:ok, matches} ->
                field = field || Enum.at(Ash.Resource.Info.primary_key(resource), 0)

                value =
                  Ash.DataLayer.Ets.aggregate_value(
                    matches,
                    kind,
                    field,
                    uniq?,
                    include_nil?,
                    default_value
                  )

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

      {:aborted, error} ->
        {:error, error}
    end
  end

  @doc false
  @impl true
  def run_query(
        %Query{
          domain: domain,
          resource: resource,
          filter: filter,
          offset: offset,
          calculations: calculations,
          limit: limit,
          sort: sort,
          aggregates: aggregates
        },
        _resource
      ) do
    with {:atomic, records} <-
           Mnesia.transaction(fn ->
             Mnesia.select(Ash.DataLayer.Ets.Info.table(resource), [{:_, [], [:"$_"]}])
           end),
         {:ok, records} <-
           records |> Enum.map(&elem(&1, 2)) |> Ash.DataLayer.Ets.cast_records(resource),
         {:ok, filtered} <- filter_matches(records, filter, domain),
         offset_records <-
           filtered |> Sort.runtime_sort(sort, domain: domain) |> Enum.drop(offset || 0),
         limited_records <- do_limit(offset_records, limit),
         {:ok, records} <-
           Ash.DataLayer.Ets.do_add_aggregates(
             limited_records,
             domain,
             resource,
             aggregates
           ),
         {:ok, records} <-
           Ash.DataLayer.Ets.do_add_calculations(
             records,
             resource,
             calculations,
             domain
           ) do
      {:ok, records}
    else
      {:error, error} ->
        {:error, error}

      {:aborted, reason} ->
        {:error, reason}
    end
  end

  defp do_limit(records, nil), do: records
  defp do_limit(records, limit), do: Enum.take(records, limit)

  defp filter_matches(records, nil, _domain), do: {:ok, records}

  defp filter_matches(records, filter, domain) do
    Ash.Filter.Runtime.filter_matches(domain, records, filter)
  end

  @doc false
  @impl true
  def create(resource, changeset) do
    {:ok, record} = Ash.Changeset.apply_attributes(changeset)

    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.map(fn attr ->
        Map.get(record, attr)
      end)

    resource
    |> Ash.Resource.Info.attributes()
    |> Map.new(&{&1.name, Map.get(record, &1.name)})
    |> Ash.DataLayer.Ets.dump_to_native(Ash.Resource.Info.attributes(resource))
    |> case do
      {:ok, values} ->
        case Mnesia.transaction(fn ->
               Mnesia.write({Ash.DataLayer.Ets.Info.table(resource), pkey, values})
             end) do
          {:atomic, _} ->
            {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

          {:aborted, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  @impl true
  def destroy(resource, %{data: record}) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.map(&Map.get(record, &1))

    result =
      Mnesia.transaction(fn ->
        Mnesia.delete({Ash.DataLayer.Ets.Info.table(resource), pkey})
      end)

    case result do
      {:atomic, _} -> :ok
      {:aborted, error} -> {:error, error}
    end
  end

  @doc false
  @impl true
  def update(resource, changeset) do
    pkey = pkey_list(resource, changeset.data)

    result =
      Mnesia.transaction(fn ->
        with {:ok, record} <- Ash.Changeset.apply_attributes(%{changeset | action_type: :update}),
             {:ok, record} <-
               do_update(Ash.DataLayer.Ets.Info.table(resource), {pkey, record}, resource),
             {:ok, record} <- Ash.DataLayer.Ets.cast_record(record, resource) do
          new_pkey = pkey_list(resource, record)

          if new_pkey != pkey do
            case destroy(resource, changeset) do
              :ok ->
                {:ok,
                 %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

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
      end)

    case result do
      {:atomic, {:error, error}} ->
        {:error, error}

      {:atomic, {:ok, result}} ->
        {:ok, result}

      {:aborted, {reason, stacktrace}} when is_exception(reason) ->
        {:error, Ash.Error.to_ash_error(reason, stacktrace)}

      {:aborted, reason} ->
        {:error, reason}
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

  defp pkey_list(resource, data) do
    resource
    |> Ash.Resource.Info.primary_key()
    |> Enum.map(&Map.get(data, &1))
  end

  defp do_update(table, {pkey, record}, resource) do
    attributes = Ash.Resource.Info.attributes(resource)

    case Ash.DataLayer.Ets.dump_to_native(record, attributes) do
      {:ok, casted} ->
        case Mnesia.read({Ash.DataLayer.Ets.Info.table(resource), pkey}) do
          [] ->
            {:error, "Record not found matching: #{inspect(pkey)}"}

          [{_, _, record}] ->
            Mnesia.write({table, pkey, Map.merge(record, casted)})
            [{_, _, record}] = Mnesia.read({table, pkey})
            {:ok, record}
        end

      {:error, error} ->
        {:error, error}
    end
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
      |> resource_to_query(changeset.domain)
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
  def transaction(_, func, _timeout, _reason) do
    case Mnesia.transaction(func, 1) do
      {:atomic, result} ->
        {:ok, result}

      {:aborted, {reason, stacktrace}} when is_exception(reason) ->
        {:error, Ash.Error.to_ash_error(reason, stacktrace)}

      {:aborted, %Ash.Changeset{} = changeset} ->
        {:error, changeset}

      {:aborted, %Ash.Query{} = query} ->
        {:error, query}

      {:aborted, reason} ->
        if Ash.Error.ash_error?(reason) do
          {:error, reason}
        else
          {:error, Ash.Error.to_ash_error(Exception.format_exit(reason))}
        end
    end
  end

  @doc false
  @impl true
  @spec rollback(term, term) :: no_return
  def rollback(_, value) do
    Mnesia.abort(value)
  end
end
