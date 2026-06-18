# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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

  require Logger

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
  def can?(_, {:atomic, :update}), do: true
  def can?(_, {:atomic, :upsert}), do: true
  def can?(_, {:atomic, :create}), do: true
  def can?(_, :through_relationship), do: true

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
    with {:atomic, records} <-
           Mnesia.transaction(fn ->
             Mnesia.select(Ash.DataLayer.Mnesia.Info.table(resource), [{:_, [], [:"$_"]}])
           end),
         {:ok, records} <-
           records |> Enum.map(&elem(&1, 2)) |> Ash.DataLayer.Ets.cast_records(resource),
         {:ok, filtered} <-
           filter_matches(records, filter, domain, tenant, context[:private][:actor]),
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

  defp filter_matches(
         records,
         filter,
         domain,
         tenant,
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

  defp filter_matches(records, filter, domain, tenant, actor, parent, conflicting_upsert_values) do
    Ash.Filter.Runtime.filter_matches(domain, records, filter,
      parent: parent,
      tenant: tenant,
      actor: actor,
      conflicting_upsert_values: conflicting_upsert_values
    )
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
    log_bulk_create(resource, stream, options)

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
                upsert_fields: options[:upsert_fields] || [],
                touch_update_defaults?: Map.get(options, :touch_update_defaults?, true)
              })
          })

        case do_upsert(resource, changeset, options.upsert_keys, nil, true) do
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
        case create(resource, changeset, with_transaction: false, from_bulk_create?: true) do
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
  def create(resource, changeset, opts \\ []) do
    if !opts[:from_bulk_create?], do: log_create(resource, changeset)

    with {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
         {:ok, record} <- apply_create_atomics(changeset, resource, record) do
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
          case do_write(
                 Ash.DataLayer.Mnesia.Info.table(resource),
                 pkey,
                 values,
                 Keyword.get(opts, :with_transaction, true)
               ) do
            # If with_transaction is false, we are in a transaction and will only return :ok
            :ok ->
              {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

            {:atomic, _} ->
              {:ok, %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}}

            {:aborted, error} ->
              {:error, error}
          end

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp apply_create_atomics(%{create_atomics: create_atomics} = changeset, resource, record)
       when create_atomics != [] do
    Enum.reduce_while(create_atomics, {:ok, record}, fn {key, expr}, {:ok, acc} ->
      case Ash.Expr.eval(expr,
             resource: resource,
             record: acc,
             domain: changeset.domain,
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

  defp apply_create_atomics(_changeset, _resource, record), do: {:ok, record}

  # This allows for writing to Mnesia without a transaction in case one was
  # started elsewhere. This was explicitly created for `bulk_create/3`.
  defp do_write(table, pkey, values, with_transaction) do
    if with_transaction && !Mnesia.is_transaction() do
      Mnesia.transaction(fn ->
        Mnesia.write({table, pkey, values})
      end)
    else
      Mnesia.write({table, pkey, values})
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
        Mnesia.delete({Ash.DataLayer.Mnesia.Info.table(resource), pkey})
      end)

    case result do
      {:atomic, _} -> :ok
      {:aborted, error} -> {:error, error}
    end
  end

  @doc false
  @impl true
  def update(resource, changeset, from_bulk_create? \\ false) do
    pkey = pkey_list(resource, changeset.data)

    if !from_bulk_create? do
      log_update(
        resource,
        Map.take(changeset.data, Ash.Resource.Info.primary_key(resource)),
        changeset
      )
    end

    result =
      Mnesia.transaction(fn ->
        with {:ok, record} <- Ash.Changeset.apply_attributes(%{changeset | action_type: :update}),
             {:ok, record} <-
               do_update(Ash.DataLayer.Mnesia.Info.table(resource), {pkey, record}, resource),
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
        case Mnesia.read({Ash.DataLayer.Mnesia.Info.table(resource), pkey}) do
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
  def upsert(resource, changeset, keys, identity \\ nil) do
    do_upsert(resource, changeset, keys, identity)
  end

  defp do_upsert(resource, changeset, keys, identity, from_bulk_create? \\ false) do
    keys = keys || Ash.Resource.Info.primary_key(resource)

    if (is_nil(identity) || !identity.nils_distinct?) &&
         Enum.any?(keys, &is_nil(Ash.Changeset.get_attribute(changeset, &1))) do
      create(resource, changeset, from_bulk_create?: from_bulk_create?)
    else
      key_filters =
        Enum.map(keys, fn key ->
          value =
            Ash.Changeset.get_attribute(changeset, key) || Map.get(changeset.params, key) ||
              Map.get(changeset.params, to_string(key))

          {key,
           if is_nil(value) do
             [is_nil: true]
           else
             value
           end}
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

      resource
      |> resource_to_query(changeset.domain)
      |> Map.put(:filter, query.filter)
      |> Map.put(:tenant, changeset.tenant)
      |> run_query(resource)
      |> case do
        {:ok, []} ->
          resource
          |> create(changeset, from_bulk_create?: from_bulk_create?)
          |> set_upsert_action(:create)

        {:ok, [result]} ->
          with {:ok, conflicting_upsert_values} <- Ash.Changeset.apply_attributes(changeset),
               {:ok, [^result]} <-
                 upsert_conflict_check(
                   changeset,
                   result,
                   conflicting_upsert_values
                 ) do
            to_set =
              changeset
              |> Ash.Changeset.set_on_upsert(keys)
              |> apply_upsert_update_defaults(resource, result, changeset)

            changeset =
              changeset
              |> Map.put(:attributes, %{})
              |> Map.put(:data, result)
              |> Ash.Changeset.force_change_attributes(to_set)

            resource
            |> update(%{changeset | action_type: :update, filter: nil}, from_bulk_create?)
            |> set_upsert_action(:update)
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

  defp set_upsert_action({:ok, record}, action) do
    {:ok, Ash.Resource.put_metadata(record, :upsert_action, action)}
  end

  defp set_upsert_action(result, _), do: result

  @spec upsert_conflict_check(
          changeset :: Ash.Changeset.t(),
          subject :: record,
          conflicting_upsert_values :: record
        ) :: {:ok, [record]} | {:error, reason}
        when record: Ash.Resource.Record.t(), reason: term()
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

  # Mnesia's update/2 calls apply_attributes which re-applies update_defaults
  # via set_defaults/3. To prevent unwanted updates, we preserve existing
  # values from the record for update_default fields so set_defaults sees
  # them as already set and skips them.
  defp apply_upsert_update_defaults(to_set, resource, existing_record, changeset) do
    touch_update_defaults? =
      changeset.context[:private][:touch_update_defaults?]

    if touch_update_defaults? == false || to_set == [] do
      upsert_fields = changeset.context[:private][:upsert_fields]

      update_default_attrs =
        resource
        |> Ash.Resource.Info.attributes()
        |> Enum.filter(& &1.update_default)

      Enum.reduce(update_default_attrs, to_set, fn attr, acc ->
        if explicitly_set?(attr.name, upsert_fields, changeset) do
          acc
        else
          Keyword.put(acc, attr.name, Map.get(existing_record, attr.name))
        end
      end)
    else
      to_set
    end
  end

  defp explicitly_set?(key, upsert_fields, _changeset) when is_list(upsert_fields),
    do: key in upsert_fields

  defp explicitly_set?(key, _, changeset),
    do: Map.has_key?(changeset.attributes, key) && key not in Map.get(changeset, :defaults, [])

  defp log_bulk_create(resource, stream, options) do
    Logger.debug(
      "Mnesia: #{bulk_create_operation(options, stream)} #{inspect(resource)}:\n\n#{Enum.map_join(stream, "\n", &format_changes(&1, from_bulk_create?: true))}"
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

  defp log_create(resource, changeset) do
    Logger.debug("""
    Mnesia: Creating #{inspect(resource)}:

    #{format_changes(changeset)}
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
    Mnesia: Updating #{inspect(resource)} #{pkey}:

    #{format_changes(changeset)}
    """)
  end

  defp format_changes(changeset, opts \\ []) do
    prefix =
      if opts[:from_bulk_create?] do
        ""
      else
        "Setting "
      end

    inspect_opts = %Inspect.Opts{
      limit: 10,
      printable_limit: 36,
      pretty: true
    }

    doc =
      Inspect.Algebra.container_doc(
        "%{",
        Enum.uniq_by(Enum.to_list(changeset.attributes) ++ changeset.atomics, &elem(&1, 0)),
        "}",
        inspect_opts,
        fn {k, v}, _opts ->
          v =
            if Ash.Expr.expr?(v) do
              Ash.Filter.map(v, fn nested ->
                truncate_unless_expr(nested, inspect_opts)
              end)
              |> inspect(
                limit: inspect_opts.limit,
                printable_limit: inspect_opts.printable_limit,
                pretty: true
              )
            else
              truncate_inspect(v, inspect_opts)
            end

          # Create the value document from the truncated string
          value_doc = Inspect.Algebra.string(v)
          Inspect.Algebra.concat([to_string(k), ": ", value_doc])
        end,
        separator: ",",
        break: :flex
      )

    # Format the document to a string with proper line breaks
    result = Inspect.Algebra.format(doc, inspect_opts.width) |> IO.iodata_to_binary()
    prefix <> result
  rescue
    e ->
      "Failed to format changes: #{Exception.message(e)}"
  end

  defp truncate_unless_expr(nested, inspect_opts) do
    if Ash.Expr.expr?(nested) do
      nested
    else
      cond do
        is_atom(nested) ->
          nested

        is_binary(nested) ->
          truncate(nested, inspect_opts)

        is_map(nested) ->
          Map.new(nested, fn {k, v} -> {k, truncate_unless_expr(v, inspect_opts)} end)

        is_list(nested) ->
          Enum.map(nested, fn v -> truncate_unless_expr(v, inspect_opts) end)

        is_tuple(nested) ->
          nested
          |> Tuple.to_list()
          |> Enum.map(&truncate_unless_expr(&1, inspect_opts))
          |> List.to_tuple()

        true ->
          truncate_inspect(nested, inspect_opts)
      end
    end
  end

  defp truncate_inspect(v, inspect_opts) do
    value_str =
      inspect(v,
        limit: inspect_opts.limit,
        printable_limit: inspect_opts.printable_limit,
        pretty: true
      )

    truncate(value_str, inspect_opts)
  end

  defp truncate(value_str, inspect_opts) do
    if String.length(value_str) > inspect_opts.printable_limit + 3 do
      String.slice(value_str, 0, inspect_opts.printable_limit) <>
        "..."
    else
      value_str
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
