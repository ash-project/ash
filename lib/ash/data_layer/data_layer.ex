defmodule Ash.DataLayer do
  @moduledoc """
  The behaviour for backing resource actions with persistence layers.
  """

  alias Spark.Dsl.Extension

  @type t :: module
  @type data_layer_query() :: struct
  @type lock_type :: :for_update | term()
  @type transaction_reason ::
          %{
            required(:type) => :create,
            required(:metadata) => %{resource: Ash.Resource.t(), action: atom},
            optional(:data_layer_context) => %{}
          }
          | %{
              required(:type) => :update,
              required(:metadata) => %{
                resource: Ash.Resource.t(),
                action: atom,
                record: Ash.Resource.record(),
                actor: term()
              },
              optional(:data_layer_context) => %{}
            }
          | %{
              required(:type) => :destroy,
              required(:metadata) => %{
                resource: Ash.Resource.t(),
                action: atom,
                record: Ash.Resource.record(),
                actor: term()
              },
              optional(:data_layer_context) => %{}
            }
          | %{
              required(:type) => :read,
              required(:metadata) => %{
                resource: Ash.Resource.t(),
                query: Ash.Query.t(),
                actor: term()
              },
              optional(:data_layer_context) => %{}
            }
          | %{
              required(:type) => :flow_transaction,
              required(:metadata) => %{
                resource: Ash.Resource.t(),
                input: Ash.ActionInput.t(),
                action: atom,
                actor: term()
              },
              optional(:data_layer_context) => %{}
            }
          | %{
              required(:type) => :generic,
              required(:metadata) => %{
                step_name: atom | list(term),
                flow: module(),
                actor: term()
              },
              optional(:data_layer_context) => %{}
            }
          | %{required(:type) => :custom, required(:metadata) => map()}
          | %{required(:type) => atom, required(:metadata) => map()}

  @type feature() ::
          :transact
          | :multitenancy
          | {:atomic, :update}
          | {:atomic, :upsert}
          | {:lateral_join, list(Ash.Resource.t())}
          | {:join, Ash.Resource.t()}
          | {:aggregate, Ash.Query.Aggregate.kind()}
          | {:aggregate_relationship, Ash.Resource.Relationships.relationship()}
          | {:query_aggregate, Ash.Query.Aggregate.kind()}
          | :select
          | :expr_error
          | :expression_calculation_sort
          | :aggregate_filter
          | :aggregate_sort
          | :boolean_filter
          | :async_engine
          | :bulk_create
          | :update_query
          | :destroy_query
          | :create
          | :read
          | :update
          | :destroy
          | :limit
          | :offset
          | :transact
          | :filter
          | :composite_type
          | {:lock, lock_type()}
          | {:filter_expr, struct}
          | {:filter_relationship, Ash.Resource.Relationships.relationship()}
          | :sort
          | {:sort, Ash.Type.t()}
          | :upsert
          | :composite_primary_key

  @type lateral_join_link ::
          {Ash.Resource.t(), atom, atom, Ash.Resource.Relationships.relationship()}

  @callback functions(Ash.Resource.t()) :: [module]
  @callback filter(data_layer_query(), Ash.Filter.t(), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback sort(data_layer_query(), Ash.Sort.t(), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback distinct_sort(data_layer_query(), Ash.Sort.t(), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback distinct(data_layer_query(), list(atom), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback prefer_lateral_join_for_many_to_many?() :: boolean
  @callback limit(
              data_layer_query(),
              limit :: non_neg_integer(),
              resource :: Ash.Resource.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback offset(
              data_layer_query(),
              offset :: non_neg_integer(),
              resource :: Ash.Resource.t()
            ) :: {:ok, data_layer_query()} | {:error, term}
  @callback select(
              data_layer_query(),
              select :: list(atom),
              resource :: Ash.Resource.t()
            ) :: {:ok, data_layer_query()} | {:error, term}
  @callback set_tenant(Ash.Resource.t(), data_layer_query(), term) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback resource_to_query(Ash.Resource.t(), Ash.Domain.t()) :: data_layer_query()
  @callback transform_query(Ash.Query.t()) :: Ash.Query.t()
  @callback run_query(data_layer_query(), Ash.Resource.t()) ::
              {:ok, list(Ash.Resource.record())} | {:error, term} | {:error, :no_rollback, term}
  @callback lock(data_layer_query(), lock_type(), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback run_aggregate_query(
              data_layer_query(),
              list(Ash.Query.Aggregate.t()),
              Ash.Resource.t()
            ) ::
              {:ok, map} | {:error, term}
  @callback run_aggregate_query_with_lateral_join(
              data_layer_query(),
              list(Ash.Query.Aggregate.t()),
              [Ash.Resource.record()],
              destination_resource :: Ash.Resource.t(),
              list(lateral_join_link())
            ) ::
              {:ok, list(Ash.Resource.t())} | {:error, term}
  @callback run_query_with_lateral_join(
              data_layer_query(),
              [Ash.Resource.record()],
              source_resource :: Ash.Resource.t(),
              list(lateral_join_link())
            ) ::
              {:ok, list(Ash.Resource.record())} | {:error, term}

  @callback return_query(data_layer_query(), Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}

  @type bulk_create_options :: %{
          batch_size: pos_integer,
          return_records?: boolean,
          upsert?: boolean,
          action_select: list(atom),
          upsert_keys: nil | list(atom),
          upsert_condition: Ash.Expr.t() | nil,
          identity: Ash.Resource.Identity.t() | nil,
          select: list(atom),
          upsert_fields:
            nil
            | list(atom)
            | :replace_all
            | {:replace, list(atom)}
            | {:replace_all_except, list(atom)},
          tenant: term()
        }

  @type bulk_update_options :: %{
          return_records?: boolean,
          action_select: list(atom),
          calculations: list({Ash.Query.Calculation.t(), Ash.Expr.t()}),
          select: list(atom),
          tenant: term()
        }

  @callback bulk_create(
              Ash.Resource.t(),
              Enumerable.t(Ash.Changeset.t()),
              options :: bulk_create_options()
            ) ::
              :ok
              | {:ok, Enumerable.t(Ash.Resource.record())}
              | {:error, Ash.Error.t()}
              | {:error, :no_rollback, Ash.Error.t()}
  @callback create(Ash.Resource.t(), Ash.Changeset.t()) ::
              {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  @callback upsert(Ash.Resource.t(), Ash.Changeset.t(), list(atom)) ::
              {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  @callback upsert(
              Ash.Resource.t(),
              Ash.Changeset.t(),
              list(atom),
              Ash.Resource.Identity.t() | nil
            ) ::
              {:ok,
               Ash.Resource.record()
               | {:upsert_skipped, Ash.Query.t(),
                  (-> {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term})}}
              | {:error, term}
              | {:error, :no_rollback, term}
  @callback update(Ash.Resource.t(), Ash.Changeset.t()) ::
              {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}

  @callback update_query(
              data_layer_query(),
              Ash.Changeset.t(),
              Ash.Resource.t(),
              opts :: bulk_update_options()
            ) ::
              :ok
              | {:ok, Enumerable.t(Ash.Resource.record())}
              | {:error, Ash.Error.t()}
              | {:error, :no_rollback, Ash.Error.t()}

  @callback destroy_query(
              data_layer_query(),
              Ash.Changeset.t(),
              Ash.Resource.t(),
              opts :: bulk_update_options()
            ) ::
              :ok
              | {:ok, Enumerable.t(Ash.Resource.record())}
              | {:error, Ash.Error.t()}
              | {:error, :no_rollback, Ash.Error.t()}

  @callback add_aggregate(
              data_layer_query(),
              Ash.Query.Aggregate.t(),
              Ash.Resource.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}

  @callback add_aggregates(
              data_layer_query(),
              list(Ash.Query.Aggregate.t()),
              Ash.Resource.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}

  @callback add_calculation(
              data_layer_query(),
              Ash.Query.Calculation.t(),
              expression :: any,
              Ash.Resource.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}

  @callback add_calculations(
              data_layer_query(),
              list({Ash.Query.Calculation.t(), expression :: any}),
              Ash.Resource.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback destroy(Ash.Resource.t(), Ash.Changeset.t()) :: :ok | {:error, term}
  @callback transaction(
              Ash.Resource.t(),
              (-> term),
              nil | pos_integer(),
              reason :: transaction_reason()
            ) ::
              {:ok, term} | {:error, term}
  @callback in_transaction?(Ash.Resource.t()) :: boolean
  @callback source(Ash.Resource.t()) :: String.t()
  @callback rollback(Ash.Resource.t(), term) :: no_return
  @callback calculate(Ash.Resource.t(), list(Ash.Expr.t()), context :: map) ::
              {:ok, term} | {:error, term}
  @callback can?(Ash.Resource.t() | Spark.Dsl.t(), feature()) :: boolean
  @callback set_context(Ash.Resource.t(), data_layer_query(), map) ::
              {:ok, data_layer_query()} | {:error, term}

  @optional_callbacks source: 1,
                      run_query: 2,
                      bulk_create: 3,
                      update_query: 4,
                      destroy_query: 4,
                      distinct: 3,
                      return_query: 2,
                      lock: 3,
                      run_query_with_lateral_join: 4,
                      create: 2,
                      update: 2,
                      set_context: 3,
                      calculate: 3,
                      destroy: 2,
                      filter: 3,
                      sort: 3,
                      distinct_sort: 3,
                      select: 3,
                      limit: 3,
                      offset: 3,
                      transaction: 4,
                      rollback: 2,
                      upsert: 3,
                      upsert: 4,
                      functions: 1,
                      in_transaction?: 1,
                      prefer_lateral_join_for_many_to_many?: 0,
                      add_aggregate: 3,
                      add_aggregates: 3,
                      add_calculation: 4,
                      add_calculations: 3,
                      run_aggregate_query: 3,
                      run_aggregate_query_with_lateral_join: 5,
                      transform_query: 1,
                      set_tenant: 3

  @doc "The data layer of the resource, or nil if it does not have one"
  @spec data_layer(Ash.Resource.t() | Spark.Dsl.t()) :: Ash.DataLayer.t() | nil
  def data_layer(resource) do
    Extension.get_persisted(resource, :data_layer)
  end

  @doc "Whether or not lateral joins should be used for many to many relationships by default"
  @spec prefer_lateral_join_for_many_to_many?(Ash.DataLayer.t()) :: boolean
  def prefer_lateral_join_for_many_to_many?(data_layer) do
    if function_exported?(data_layer, :prefer_lateral_join_for_many_to_many?, 0) do
      data_layer.prefer_lateral_join_for_many_to_many?()
    else
      true
    end
  end

  @doc "Whether or not the data layer supports a specific feature"
  @spec data_layer_can?(Ash.Resource.t() | Spark.Dsl.t(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && Ash.DataLayer.can?(feature, resource)
  end

  @doc "Custom functions supported by the data layer of the resource"
  @spec data_layer_functions(Ash.Resource.t()) :: map
  def data_layer_functions(resource) do
    Ash.DataLayer.functions(resource)
  end

  @spec calculate(Ash.Resource.t(), list(Ash.Expr.t()), context :: map) ::
          {:ok, list(term)} | {:error, Ash.Error.t()}
  def calculate(resource, exprs, context) do
    data_layer(resource).calculate(resource, exprs, context)
  end

  @doc "Wraps the execution of the function in a transaction with the resource's data_layer"
  @spec transaction(
          Ash.Resource.t() | [Ash.Resource.t()],
          (-> term),
          nil | pos_integer(),
          reason :: transaction_reason()
        ) :: term

  def transaction(
        resource_or_resources,
        func,
        timeout \\ nil,
        reason \\ %{type: :custom, metadata: %{}}
      )

  def transaction([], func, _, _reason) do
    {:ok, func.()}
  end

  def transaction([resource], func, timeout, reason) do
    transaction(resource, func, timeout, reason)
  end

  def transaction([resource | resources], func, timeout, reason) do
    transaction(
      resource,
      fn ->
        case transaction(resources, func, timeout, reason) do
          {:ok, result} ->
            result

          {:error, error} ->
            rollback(resource, error)
        end
      end,
      timeout,
      reason
    )
  end

  def transaction(resource, func, timeout, reason) do
    if in_transaction?(resource) do
      {:ok, func.()}
    else
      data_layer = data_layer(resource)

      if data_layer.can?(resource, :transact) do
        cond do
          function_exported?(data_layer, :transaction, 4) ->
            data_layer.transaction(resource, func, timeout, reason)

          function_exported?(data_layer, :transaction, 3) ->
            data_layer.transaction(resource, func, timeout)

          true ->
            data_layer.transaction(resource, func)
        end
      else
        {:ok, func.()}
      end
    end
  end

  @doc "Rolls back the current transaction"
  @spec rollback(Ash.Resource.t() | list(Ash.Resource.t()), term) :: no_return
  def rollback([resource | _], term) do
    rollback(resource, term)
  end

  def rollback(resource, term) do
    data_layer(resource).rollback(resource, term)
  end

  @spec resource_to_query(Ash.Resource.t(), Ash.Domain.t()) :: data_layer_query()
  def resource_to_query(resource, domain) do
    Ash.DataLayer.data_layer(resource).resource_to_query(resource, domain)
  end

  @spec update(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def update(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}
    Ash.DataLayer.data_layer(resource).update(resource, changeset)
  end

  @spec update_query(data_layer_query(), Ash.Changeset.t(), opts :: bulk_update_options()) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def update_query(query, changeset, opts) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    Ash.DataLayer.data_layer(changeset.resource).update_query(
      query,
      changeset,
      changeset.resource,
      opts
    )
  end

  @spec destroy_query(data_layer_query(), Ash.Changeset.t(), opts :: bulk_update_options()) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def destroy_query(query, changeset, opts) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    Ash.DataLayer.data_layer(changeset.resource).destroy_query(
      query,
      changeset,
      changeset.resource,
      opts
    )
  end

  @spec create(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def create(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}
    Ash.DataLayer.data_layer(resource).create(resource, changeset)
  end

  @spec return_query(data_layer_query(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def return_query(query, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if function_exported?(data_layer, :return_query, 2) do
      data_layer.return_query(query, resource)
    else
      {:ok, query}
    end
  end

  @spec bulk_create(
          Ash.Resource.t(),
          Enumerable.t(Ash.Changeset.t()),
          options :: bulk_create_options
        ) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def bulk_create(resource, changesets, options) do
    Ash.DataLayer.data_layer(resource).bulk_create(resource, changesets, options)
  end

  @spec destroy(Ash.Resource.t(), Ash.Changeset.t()) ::
          :ok | {:error, term} | {:error, :no_rollback, term}
  def destroy(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    Ash.DataLayer.data_layer(resource).destroy(resource, changeset)
  end

  @spec source(Ash.Resource.t()) :: String.t()
  def source(resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    Code.ensure_compiled!(data_layer)

    if :erlang.function_exported(data_layer, :source, 1) do
      data_layer.source(resource)
    else
      ""
    end
  end

  @spec set_tenant(Ash.Resource.t(), data_layer_query(), String.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_tenant(resource, query, term) do
    Ash.DataLayer.data_layer(resource).set_tenant(resource, query, term)
  end

  @spec upsert(
          Ash.Resource.t(),
          Ash.Changeset.t(),
          list(atom),
          identity :: Ash.Resource.Identity.t() | nil
        ) ::
          {:ok,
           Ash.Resource.record()
           | {:upsert_skipped, Ash.Query.t(),
              (-> {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term})}}
          | {:error, term}
          | {:error, :no_rollback, term}

  def upsert(resource, changeset, keys, identity \\ nil) do
    changeset = %{changeset | tenant: changeset.to_tenant}
    data_layer = Ash.DataLayer.data_layer(resource)

    if function_exported?(data_layer, :upsert, 4) do
      data_layer.upsert(resource, changeset, keys, identity)
    else
      data_layer.upsert(resource, changeset, keys)
    end
  end

  @spec set_context(Ash.Resource.t(), data_layer_query(), map) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_context(resource, query, map) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if :erlang.function_exported(data_layer, :set_context, 3) do
      data_layer.set_context(resource, query, map)
    else
      {:ok, query}
    end
  end

  @spec filter(data_layer_query(), Ash.Filter.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def filter(query, nil, _), do: {:ok, query}

  def filter(query, filter, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if data_layer.can?(resource, :filter) do
      if data_layer.can?(resource, :boolean_filter) do
        data_layer.filter(query, filter, resource)
      else
        simple_filter = Ash.Filter.to_simple_filter(filter)
        data_layer.filter(query, simple_filter, resource)
      end
    else
      {:error, "Data layer does not support filtering"}
    end
  end

  @spec sort(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def sort(query, sort, resource) do
    if can?(:sort, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.sort(query, sort, resource)
    else
      {:ok, query}
    end
  end

  @spec distinct(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct(query, distinct, resource) do
    if can?(:distinct, resource) && distinct do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.distinct(query, distinct, resource)
    else
      {:ok, query}
    end
  end

  @spec distinct_sort(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct_sort(query, sort, resource) do
    if can?(:distinct_sort, resource) && sort do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.distinct_sort(query, sort, resource)
    else
      {:ok, query}
    end
  end

  @spec limit(data_layer_query(), limit :: non_neg_integer, Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def limit(query, nil, _resource), do: {:ok, query}

  def limit(query, limit, resource) do
    if can?(:limit, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.limit(query, limit, resource)
    else
      {:ok, query}
    end
  end

  @spec offset(data_layer_query(), offset :: non_neg_integer, Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def offset(query, nil, _resource), do: {:ok, query}

  def offset(query, offset, resource) do
    if can?(:offset, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.offset(query, offset, resource)
    else
      {:ok, query}
    end
  end

  @spec select(data_layer_query(), select :: list(atom), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def select(query, nil, _resource), do: {:ok, query}

  def select(query, select, resource) do
    if can?(:select, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.select(query, select, resource)
    else
      {:ok, query}
    end
  end

  @spec add_aggregates(data_layer_query(), list(Ash.Query.Aggregate.t()), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_aggregates(query, [], _) do
    {:ok, query}
  end

  def add_aggregates(query, aggregates, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if function_exported?(data_layer, :add_aggregates, 3) do
      data_layer.add_aggregates(query, aggregates, resource)
    else
      Enum.reduce_while(aggregates, {:ok, query}, fn aggregate, {:ok, data_layer_query} ->
        case data_layer.add_aggregate(data_layer_query, aggregate, resource) do
          {:ok, data_layer_query} -> {:cont, {:ok, data_layer_query}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)
    end
  end

  @spec add_calculations(
          data_layer_query(),
          list({Ash.Query.Calculation.t(), expression :: term}),
          Ash.Resource.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_calculations(query, [], _) do
    {:ok, query}
  end

  def add_calculations(query, calculations, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if function_exported?(data_layer, :add_calculations, 3) do
      data_layer.add_calculations(query, calculations, resource)
    else
      Enum.reduce_while(calculations, {:ok, query}, fn {calculation, expression}, {:ok, query} ->
        case data_layer.add_calculation(query, calculation, expression, resource) do
          {:ok, query} -> {:cont, {:ok, query}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)
    end
  end

  @spec can?(feature, Ash.Resource.t() | Spark.Dsl.t()) :: boolean
  def can?(feature, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)
    data_layer.can?(resource, feature)
  end

  @spec run_aggregate_query(
          data_layer_query(),
          list(Ash.Query.Aggregate.t()),
          Ash.Resource.t()
        ) ::
          {:ok, map} | {:error, term}
  def run_aggregate_query(query, aggregates, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if :erlang.function_exported(data_layer, :run_aggregate_query, 3) do
      data_layer.run_aggregate_query(query, aggregates, resource)
    else
      {:error, "Aggregate queries not supported"}
    end
  end

  @spec run_query(data_layer_query(), central_resource :: Ash.Resource.t()) ::
          {:ok, list(Ash.Resource.record())} | {:error, term} | {:error, :no_rollback, term}
  def run_query(query, central_resource) do
    Ash.DataLayer.data_layer(central_resource).run_query(query, central_resource)
  end

  @spec lock(data_layer_query(), lock_type :: lock_type() | nil, resource :: Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def lock(query, nil, _), do: {:ok, query}

  def lock(query, lock_type, resource) do
    Ash.DataLayer.data_layer(resource).lock(query, lock_type, resource)
  end

  def run_aggregate_query_with_lateral_join(
        query,
        aggregates,
        root_data,
        destination_resource,
        path
      ) do
    Ash.DataLayer.data_layer(destination_resource).run_aggregate_query_with_lateral_join(
      query,
      aggregates,
      root_data,
      destination_resource,
      path
    )
  end

  def run_query_with_lateral_join(
        query,
        root_data,
        destination_resource,
        path
      ) do
    Ash.DataLayer.data_layer(destination_resource).run_query_with_lateral_join(
      query,
      root_data,
      destination_resource,
      path
    )
  end

  def in_transaction?(resource) do
    if can?(:transact, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.in_transaction?(resource)
    else
      false
    end
  end

  def functions(resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if :erlang.function_exported(data_layer, :functions, 1) do
      data_layer.functions(resource)
    else
      %{}
    end
  end

  def transform_query(query) do
    data_layer = Ash.DataLayer.data_layer(query.resource)

    if :erlang.function_exported(data_layer, :transform_query, 1) do
      data_layer.transform_query(query)
    else
      query
    end
  end
end
