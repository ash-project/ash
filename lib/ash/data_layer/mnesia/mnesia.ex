defmodule Ash.DataLayer.Mnesia do
  @behaviour Ash.DataLayer
  require Logger

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

  ## Performance

  This data layer uses Mnesia matchspecs to push filter operations down to the database
  level when possible. Supported filters (equality, comparison, boolean logic, etc.) are
  converted to matchspecs for efficient querying. Unsupported filters fall back to
  runtime filtering in memory. Due to this limitation, unsupported filtering may
  have an effect on performance as filtering occurs in memory. For more on the
  supported options see `Ash.DataLayer.Mnesia.MatchSpec`.
  """

  use Spark.Dsl.Extension,
    sections: [@mnesia],
    persisters: [Ash.DataLayer.Mnesia.Transformers.DefineRecords],
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
    |> Enum.each(fn resource ->
      table = Ash.DataLayer.Mnesia.Info.table(resource)
      attributes = resource.mnesia_record_info()
      # TODO: Implement configurable type
      Mnesia.create_table(table, attributes: attributes, type: :ordered_set)
    end)
  end

  defmodule Query do
    @moduledoc false
    defstruct [
      :domain,
      :resource,
      :filter,
      :limit,
      :tenant,
      sort: [],
      context: %{},
      relationships: %{},
      offset: 0,
      aggregates: [],
      calculations: []
    ]
  end

  @doc false
  @impl true
  def can?(_, :async_engine), do: true
  def can?(_, :bulk_create), do: true
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
  def can?(_, {:aggregate, :unrelated}), do: true
  def can?(_, {:exists, :unrelated}), do: true
  def can?(resource, {:query_aggregate, kind}), do: can?(resource, {:aggregate, kind})

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

  def in_transaction?, do: Mnesia.is_transaction()

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
            |> filter_matches(
              Map.get(query || %{}, :filter),
              domain,
              query.tenant,
              query.context[:private][:actor]
            )
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
      {:ok, acc} ->
        {:ok, acc}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}

      {:aborted, error} ->
        {:error, error}
    end
  end

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
  def run_query(
        %Query{
          domain: domain,
          resource: resource,
          filter: filter,
          offset: offset,
          calculations: calculations,
          limit: limit,
          sort: sort,
          aggregates: aggregates,
          tenant: tenant,
          context: context
        },
        _resource
      ) do
    # Build matchspec from filter if possible. If this fails, we will fall back
    # to runtime filtering. This will only happen for the small subset of
    # filters we have not implemented.
    {matchspec_status, matchspec} = build_matchspec(filter)

    with {:atomic, records} <-
           Mnesia.transaction(fn ->
             Mnesia.select(Ash.DataLayer.Mnesia.Info.table(resource), matchspec)
           end),
         # Convert to Ash.Resource from Erlang Records
         {:ok, records} <-
           records
           |> Enum.map(&resource.from_ex_record(&1))
           |> then(fn records -> {:ok, records} end),
         # Do runtime filtering if our matchspec failed
         {:ok, filtered} <-
           filter_if_not_using_matchspec(
             matchspec_status,
             records,
             filter,
             domain,
             tenant,
             context[:private][:actor]
           ),
         # Runtime sort, offset, and limit
         #
         # TODO: I think we can implement this in Mnesia, but we need to figure
         # out all of the details based on the table types (bag, set,
         # ordered_set)
         offset_records <-
           filtered |> Sort.runtime_sort(sort, domain: domain) |> Enum.drop(offset || 0),
         limited_records <- do_limit(offset_records, limit),
         # Add aggregates
         {:ok, records} <-
           Ash.DataLayer.Ets.do_add_aggregates(
             limited_records,
             domain,
             resource,
             aggregates
           ),
         # Add calculations
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

  defp filter_if_not_using_matchspec(matchspec_status, records, filter, domain, tenant, actor) do
    if matchspec_status == :ok do
      {:ok, records}
    else
      filter_matches(records, filter, domain, tenant, actor)
    end
  end

  defp build_matchspec(filter) do
    case Ash.DataLayer.Mnesia.MatchSpec.to_matchspec(filter) do
      {:ok, matchspec} ->
        {:ok, matchspec}

      {:error, reason} ->
        Logger.debug("Unable to convert filter to matchspec: #{reason}. Using runtime filtering.")
        {:ok, matchspec} = Ash.DataLayer.Mnesia.MatchSpec.to_matchspec(nil)
        {:runtime, matchspec}
    end
  end

  defp do_limit(records, nil), do: records
  defp do_limit(records, limit), do: Enum.take(records, limit)

  defp filter_matches(records, nil, _domain, _tenant, _), do: {:ok, records}

  defp filter_matches(records, filter, domain, tenant, actor) do
    Ash.Filter.Runtime.filter_matches(domain, records, filter, tenant: tenant, actor: actor)
  end

  @doc """
  Bulk create records in the database.

  This function is used to create multiple records in a single transaction.

  If you are NOT setting the `upsert? = true` option, this will be optimized by
  creating a single transaction and bulk creating all of the entries. The way
  :mnesia.write works, will effectively do an upsert, but you cannot control
  which fields are updated and the only identity you are matching on is the
  primary key.

  If you are using an `upsert?` it will be unoptimized and will load all records
  into memory before performing the upsert operation.
  """
  @impl true
  def bulk_create(resource, stream, options) do
    stream = Enum.to_list(stream)

    if options[:upsert?] do
      # This uses a lot of the ETS datalayer as a reference point. It is
      # recommended to NOT use this and instead upsert on the pkey which should
      # use the optmized approach in the `else` logic.
      stream
      |> Enum.reduce_while({:ok, []}, fn changeset, {:ok, results} ->
        changeset =
          Ash.Changeset.set_context(changeset, %{
            private:
              Map.merge(changeset.context[:private] || %{}, %{
                upsert_fields: options[:upsert_fields] || []
              })
          })

        case upsert(resource, changeset, options.upsert_keys) do
          {:ok, result} ->
            result =
              if options[:return_records?] do
                Ash.Resource.put_metadata(
                  result,
                  :bulk_create_index,
                  changeset.context.bulk_create.index
                )
              else
                result
              end

            {:cont, {:ok, [result | results]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, results} ->
          if options[:return_records?] do
            {:ok, Enum.reverse(results)}
          else
            :ok
          end

        {:error, error} ->
          {:error, error}
      end
    else
      do_bulk_write(resource, stream, options)
      |> case do
        {:atomic, {:ok, results}} ->
          if options[:return_records?] do
            {:ok, Enum.reverse(results)}
          else
            :ok
          end

        {:aborted, {:error, error}} ->
          {:error, error}
      end
    end
  end

  # Does a bulk write using a single transaction
  defp do_bulk_write(resource, stream, options) do
    Mnesia.transaction(fn ->
      Enum.reduce_while(stream, {:ok, []}, fn changeset, {:ok, results} ->
        # Sending in `false` prevents a transaction for every write
        case create(resource, changeset) do
          {:ok, result} ->
            result =
              if options[:return_records?] do
                Ash.Resource.put_metadata(
                  result,
                  :bulk_create_index,
                  changeset.context.bulk_create.index
                )
              else
                result
              end

            {:cont, {:ok, [result | results]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
    end)
  end

  @doc false
  @impl true
  def create(resource, changeset) do
    {:ok, record} = Ash.Changeset.apply_attributes(changeset)

    ex_record = resource.to_ex_record(record)

    case do_write(fn ->
           Mnesia.write(ex_record)
         end) do
      :ok ->
        {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

      {:atomic, :ok} ->
        {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

      {:aborted, reason} ->
        {:error, reason}
    end
  end

  # This allows for writing to Mnesia without a transaction in case one was
  # started elsewhere. This was explicitly created for `bulk_create/3`.
  defp do_write(write_fn) do
    if in_transaction?() do
      write_fn.()
    else
      Mnesia.transaction(fn ->
        write_fn.()
      end)
    end
  end

  @doc false
  @impl true
  def destroy(resource, %{data: record}) do
    pkey =
      resource
      |> Ash.Resource.Info.primary_key()
      |> Enum.map(&Map.get(record, &1))
      |> case do
        [value] -> value
        [first | rest] -> [first | rest] |> List.to_tuple()
      end

    result =
      Mnesia.transaction(fn ->
        Mnesia.delete({Ash.DataLayer.Mnesia.Info.table(resource), pkey})
      end)

    case result do
      {:atomic, _} -> :ok
      {:aborted, error} -> {:error, error}
    end
  end

  @doc false
  @impl true
  def update(resource, changeset) do
    {:ok, pkey} = pkey_val(resource, changeset.data)

    result =
      Mnesia.transaction(fn ->
        with {:ok, record} <- Ash.Changeset.apply_attributes(%{changeset | action_type: :update}),
             {:ok, record} <-
               do_update(Ash.DataLayer.Mnesia.Info.table(resource), {pkey, record}, resource),
             {:ok, record} <-
               resource.from_ex_record(record)
               |> then(&{:ok, &1}) do
          {:ok, new_pkey} = pkey_val(resource, record)

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

  # This will return primary keys in the way that Mnesia expects them. If there
  # is a single primary key, it will be returned as a single value. If there are
  # multiple primary keys, they will be returned as a tuple. This will always be
  # stored as the first value in an Mnesia record.
  defp pkey_val(resource, data) do
    resource
    |> Ash.Resource.Info.primary_key()
    |> Enum.map(&Map.get(data, &1))
    |> case do
      [pkey] ->
        {:ok, pkey}

      [_ | _] = pkeys ->
        {:ok, List.to_tuple(pkeys)}

      _ ->
        {:error, "Invalid primary key"}
    end
  end

  defp do_update(table, {pkey, record}, resource) do
    case Mnesia.read({table, pkey}) do
      [] ->
        {:error, "Record not found matching: #{inspect(pkey)}"}

      [ex_record] when is_tuple(ex_record) ->
        old_record = resource.from_ex_record(ex_record)
        new_record = Map.merge(old_record, record)
        new_ex_record = resource.to_ex_record(new_record)
        :ok = Mnesia.write(new_ex_record)
        {:ok, new_ex_record}
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

          # TODO: we are fetching the record above, but `update/2` is going to
          # fetch it again. This should be optimized to avoid redundant fetches.
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
