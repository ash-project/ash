defmodule Ash.DataLayer do
  @moduledoc """
  The interface for being an ash data layer.

  This is a large behaviour, and this capability is not complete, but the idea
  is to have a large amount of optional callbacks, and use the `can?/2` callback
  to ensure that the engine only ever tries to interact with the data layer in ways
  that it supports.
  """

  alias Spark.Dsl.Extension

  @type t :: module
  @type data_layer_query() :: struct
  @type transaction_reason ::
          %{type: :create, metadata: %{resource: Ash.Resource.t(), action: atom}}
          | %{
              type: :update,
              metadata: %{
                resource: Ash.Resource.t(),
                action: atom,
                record: Ash.Resource.record(),
                actor: term()
              }
            }
          | %{
              type: :destroy,
              metadata: %{
                resource: Ash.Resource.t(),
                action: atom,
                record: Ash.Resource.record(),
                actor: term()
              }
            }
          | %{
              type: :read,
              metadata: %{resource: Ash.Resource.t(), query: Ash.Query.t(), actor: term()}
            }
          | %{
              type: :flow_transaction,
              metadata: %{step_name: atom | list(term), flow: module(), actor: term()}
            }
          | %{type: :custom, metadata: map()}
          | %{type: atom, metadata: map()}

  @type feature() ::
          :transact
          | :multitenancy
          | {:lateral_join, list(Ash.Resource.t())}
          | {:join, Ash.Resource.t()}
          | {:aggregate, Ash.Query.Aggregate.kind()}
          | {:aggregate_relationship, Ash.Resource.Relationships.relationship()}
          | {:query_aggregate, Ash.Query.Aggregate.kind()}
          | :select
          | :aggregate_filter
          | :aggregate_sort
          | :boolean_filter
          | :async_engine
          | :create
          | :read
          | :update
          | :destroy
          | :limit
          | :offset
          | :transact
          | :filter
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
  @callback distinct(data_layer_query(), list(atom), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}
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
  @callback resource_to_query(Ash.Resource.t(), Ash.Api.t()) :: data_layer_query()
  @callback transform_query(Ash.Query.t()) :: Ash.Query.t()
  @callback run_query(data_layer_query(), Ash.Resource.t()) ::
              {:ok, list(Ash.Resource.record())} | {:error, term}
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
              {:ok, list(Ash.Resource.t())} | {:error, term}
  @callback create(Ash.Resource.t(), Ash.Changeset.t()) ::
              {:ok, Ash.Resource.t()} | {:error, term}
  @callback upsert(Ash.Resource.t(), Ash.Changeset.t(), list(atom)) ::
              {:ok, Ash.Resource.t()} | {:error, term}
  @callback update(Ash.Resource.t(), Ash.Changeset.t()) ::
              {:ok, Ash.Resource.t()} | {:error, term}
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
              (() -> term),
              nil | pos_integer(),
              reason :: transaction_reason()
            ) ::
              {:ok, term} | {:error, term}
  @callback in_transaction?(Ash.Resource.t()) :: boolean
  @callback source(Ash.Resource.t()) :: String.t()
  @callback rollback(Ash.Resource.t(), term) :: no_return
  @callback can?(Ash.Resource.t(), feature()) :: boolean
  @callback set_context(Ash.Resource.t(), data_layer_query(), map) ::
              {:ok, data_layer_query()} | {:error, term}

  @optional_callbacks source: 1,
                      run_query: 2,
                      distinct: 3,
                      run_query_with_lateral_join: 4,
                      create: 2,
                      update: 2,
                      set_context: 3,
                      destroy: 2,
                      filter: 3,
                      sort: 3,
                      select: 3,
                      limit: 3,
                      offset: 3,
                      transaction: 4,
                      rollback: 2,
                      upsert: 3,
                      functions: 1,
                      in_transaction?: 1,
                      add_aggregate: 3,
                      add_aggregates: 3,
                      add_calculation: 4,
                      add_calculations: 3,
                      run_aggregate_query: 3,
                      run_aggregate_query_with_lateral_join: 5,
                      transform_query: 1,
                      set_tenant: 3

  @doc "The data layer of the resource, or nil if it does not have one"
  @spec data_layer(Ash.Resource.t()) :: Ash.DataLayer.t()
  def data_layer(resource) do
    Extension.get_persisted(resource, :data_layer)
  end

  @doc "Whether or not the data layer supports a specific feature"
  @spec data_layer_can?(Ash.Resource.t(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && Ash.DataLayer.can?(feature, resource)
  end

  @doc "Custom functions supported by the data layer of the resource"
  @spec data_layer_functions(Ash.Resource.t()) :: map
  def data_layer_functions(resource) do
    Ash.DataLayer.functions(resource)
  end

  @doc "Wraps the execution of the function in a transaction with the resource's data_layer"
  @spec transaction(
          Ash.Resource.t() | [Ash.Resource.t()],
          (() -> term),
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
        transaction(resources, func, timeout, reason)
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
  @spec rollback(Ash.Resource.t(), term) :: no_return
  def rollback(resource, term) do
    data_layer(resource).rollback(resource, term)
  end

  @spec resource_to_query(Ash.Resource.t(), Ash.Api.t()) :: data_layer_query()
  def resource_to_query(resource, api) do
    Ash.DataLayer.data_layer(resource).resource_to_query(resource, api)
  end

  @spec update(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def update(resource, changeset) do
    Ash.DataLayer.data_layer(resource).update(resource, changeset)
  end

  @spec create(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def create(resource, changeset) do
    Ash.DataLayer.data_layer(resource).create(resource, changeset)
  end

  @spec destroy(Ash.Resource.t(), Ash.Changeset.t()) :: :ok | {:error, term}
  def destroy(resource, changeset) do
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

  @spec set_tenant(Ash.Resource.t(), data_layer_query(), term) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_tenant(resource, query, term) do
    Ash.DataLayer.data_layer(resource).set_tenant(resource, query, term)
  end

  @spec upsert(Ash.Resource.t(), Ash.Changeset.t(), list(atom)) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def upsert(resource, changeset, keys) do
    Ash.DataLayer.data_layer(resource).upsert(resource, changeset, keys)
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

  @spec distinct(data_layer_query(), list(atom) | nil, Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct(query, distinct, resource) do
    if can?(:distinct, resource) && distinct do
      data_layer = Ash.DataLayer.data_layer(resource)
      data_layer.distinct(query, distinct, resource)
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

  @spec can?(feature, Ash.Resource.t()) :: boolean
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
          {:ok, list(Ash.Resource.record())} | {:error, term}
  def run_query(query, central_resource) do
    Ash.DataLayer.data_layer(central_resource).run_query(query, central_resource)
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
