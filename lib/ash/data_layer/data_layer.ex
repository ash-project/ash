# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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

  @type combination_type :: :union | :union_all | :intersection

  @type feature() ::
          :transact
          | :multitenancy
          | :combine
          | {:combine, combination_type}
          | {:atomic, :update}
          | {:atomic, :upsert}
          | {:atomic, :create}
          | {:exists, :unrelated}
          | {:lateral_join, list(Ash.Resource.t())}
          | {:join, Ash.Resource.t()}
          | {:aggregate, :unrelated}
          | {:aggregate, Ash.Query.Aggregate.kind()}
          | {:aggregate_relationship, Ash.Resource.Relationships.relationship()}
          | {:query_aggregate, Ash.Query.Aggregate.kind()}
          | :select
          | :expr_error
          | :calculate
          | :expression_calculation
          | :expression_calculation_sort
          | :aggregate_filter
          | :aggregate_sort
          | :boolean_filter
          | :async_engine
          | :bulk_create
          | :bulk_create_with_partial_success
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
          | :through_relationship
          | :bulk_upsert_return_skipped

  @type lateral_join_link ::
          {Ash.Resource.t(), atom, atom, Ash.Resource.Relationships.relationship()}

  @callback functions(Ash.Resource.t()) :: [module]
  @callback combination_of(
              combine :: [{combination_type, data_layer_query()}],
              resource :: Ash.Resource.t(),
              domain :: Ash.Domain.t()
            ) ::
              {:ok, data_layer_query()} | {:error, term}
  @callback filter(data_layer_query(), Ash.Filter.t(), resource :: Ash.Resource.t()) ::
              {:ok, data_layer_query()} | {:error, term}

  @callback combination_acc(data_layer_query()) :: any
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
          return_skipped_upsert?: boolean,
          identity: Ash.Resource.Identity.t() | nil,
          select: list(atom),
          upsert_fields:
            nil
            | list(atom)
            | :replace_all
            | {:replace, list(atom)}
            | {:replace_all_except, list(atom)},
          touch_update_defaults?: boolean,
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
              | {:partial_success,
                 failed :: Enumerable.t({error :: term(), changeset :: Ash.Changeset.t()}),
                 Enumerable.t(Ash.Resource.record())}
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
  @callback destroy(Ash.Resource.t(), Ash.Changeset.t()) ::
              :ok | {:error, term} | {:error, :no_rollback, term}
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
  @callback prefer_transaction?(Ash.Resource.t()) :: boolean
  @callback prefer_transaction_for_atomic_updates?(Ash.Resource.t()) :: boolean
  @callback can?(Ash.Resource.t() | Spark.Dsl.t(), feature()) :: boolean
  @callback set_context(Ash.Resource.t(), data_layer_query(), map) ::
              {:ok, data_layer_query()} | {:error, term}

  @optional_callbacks source: 1,
                      combination_acc: 1,
                      run_query: 2,
                      bulk_create: 3,
                      update_query: 4,
                      destroy_query: 4,
                      distinct: 3,
                      return_query: 2,
                      lock: 3,
                      run_query_with_lateral_join: 4,
                      combination_of: 3,
                      create: 2,
                      update: 2,
                      set_context: 3,
                      prefer_transaction?: 1,
                      prefer_transaction_for_atomic_updates?: 1,
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
    if Code.ensure_loaded?(data_layer) &&
         function_exported?(data_layer, :prefer_lateral_join_for_many_to_many?, 0) do
      result = apply(data_layer, :prefer_lateral_join_for_many_to_many?, [])

      if is_boolean(result) do
        result
      else
        raise Ash.Error.Framework.InvalidReturnType,
          message: """
          Invalid value returned from #{inspect(data_layer)}.prefer_lateral_join_for_many_to_many?/0.
          The callback #{inspect(__MODULE__)}.prefer_lateral_join_for_many_to_many?/0 expects a boolean.
          """
      end
    else
      true
    end
  end

  @doc "Whether or not the data layer supports a specific feature"
  @spec data_layer_can?(Ash.Resource.t() | Spark.Dsl.t(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && can?(data_layer, resource, feature)
  end

  @doc "Custom functions supported by the data layer of the resource"
  @spec data_layer_functions(Ash.Resource.t()) :: map
  def data_layer_functions(resource) do
    Ash.DataLayer.functions(resource)
  end

  @spec calculate(Ash.Resource.t(), list(Ash.Expr.t()), context :: map) ::
          {:ok, list(term)} | {:error, Ash.Error.t()}
  def calculate(resource, exprs, context) do
    calculate(Ash.DataLayer.data_layer(resource), resource, exprs, context)
  end

  @doc false
  @spec calculate(module(), Ash.Resource.t(), list(Ash.Expr.t()), map()) ::
          {:ok, list(term)} | {:error, Ash.Error.t()}
  def calculate(data_layer_module, resource, exprs, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :calculate,
      [resource, exprs, context],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "calculate/3"
    )
  end

  @spec prefer_transaction?(Ash.Resource.t()) :: boolean
  def prefer_transaction?(resource) do
    data_layer = data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :prefer_transaction?, 1) do
      prefer_transaction?(data_layer, resource)
    else
      # default to false in 4.0
      # also change in postgres data layer to default to false
      true
    end
  end

  @doc false
  @spec prefer_transaction?(module(), Ash.Resource.t()) :: boolean
  def prefer_transaction?(data_layer_module, resource) do
    result = apply(data_layer_module, :prefer_transaction?, [resource])

    if is_boolean(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.prefer_transaction?/1.
        The callback #{inspect(__MODULE__)}.prefer_transaction?/1 expects a boolean.
        """
    end
  end

  @spec prefer_transaction_for_atomic_updates?(Ash.Resource.t()) :: boolean
  def prefer_transaction_for_atomic_updates?(resource) do
    data_layer = data_layer(resource)

    if Code.ensure_loaded?(data_layer) &&
         function_exported?(data_layer, :prefer_transaction_for_atomic_updates?, 1) do
      prefer_transaction_for_atomic_updates?(data_layer, resource)
    else
      # default to false in 4.0
      # also change in postgres data layer to default to false
      true
    end
  end

  @doc false
  @spec prefer_transaction_for_atomic_updates?(module(), Ash.Resource.t()) :: boolean
  def prefer_transaction_for_atomic_updates?(data_layer_module, resource) do
    result = apply(data_layer_module, :prefer_transaction_for_atomic_updates?, [resource])

    if is_boolean(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.prefer_transaction_for_atomic_updates?/1.
        The callback #{inspect(__MODULE__)}.prefer_transaction_for_atomic_updates?/1 expects a boolean.
        """
    end
  end

  @doc """
  Wraps the execution of the function in a transaction with the resource's data_layer.
  """
  @spec transaction(
          Ash.Resource.t() | [Ash.Resource.t()],
          (-> term),
          nil | pos_integer(),
          reason :: transaction_reason(),
          opts :: Keyword.t()
        ) :: term

  def transaction(
        resource_or_resources,
        func,
        timeout \\ nil,
        reason \\ %{type: :custom, metadata: %{}},
        opts \\ []
      )

  def transaction([], func, _, _reason, _opts) do
    {:ok, func.()}
  end

  def transaction([resource], func, timeout, reason, opts) do
    transaction(resource, func, timeout, reason, opts)
  end

  def transaction([resource | resources], func, timeout, reason, opts) do
    transaction(
      resource,
      fn ->
        case transaction(resources, func, timeout, reason, opts) do
          {:ok, result} ->
            result

          {:error, error} ->
            rollback(resource, error)
        end
      end,
      timeout,
      reason,
      opts
    )
  end

  def transaction(resource, func, timeout, reason, opts) do
    rollback_on_error? =
      Keyword.get(
        opts,
        :rollback_on_error?,
        Application.get_env(:ash, :transaction_rollback_on_error?, false)
      )

    callback = fn ->
      case {func.(), rollback_on_error?} do
        {{:error, error} = result, true} ->
          rollback(resource, error)
          result

        {result, _} ->
          result
      end
    end

    if in_transaction?(resource) do
      {:ok, callback.()}
    else
      data_layer = data_layer(resource)

      if can?(data_layer, resource, :transact) do
        cond do
          !Code.ensure_loaded?(data_layer) ->
            run_transaction(data_layer, resource, callback)

          function_exported?(data_layer, :transaction, 4) ->
            run_transaction(data_layer, resource, callback, timeout, reason)

          function_exported?(data_layer, :transaction, 3) ->
            run_transaction(data_layer, resource, callback, timeout)

          true ->
            run_transaction(data_layer, resource, callback)
        end
      else
        {:ok, func.()}
      end
    end
  end

  @doc false
  @spec run_transaction(module(), Ash.Resource.t(), (-> term)) :: {:ok, term} | {:error, term}
  def run_transaction(data_layer_module, resource, callback) do
    result = apply(data_layer_module, :transaction, [resource, callback])

    if match?({:ok, _}, result) or match?({:error, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.transaction/2.
        The callback #{inspect(__MODULE__)}.transaction/4 expects {:ok, term} or {:error, term}.
        """
    end
  end

  @doc false
  @spec run_transaction(module(), Ash.Resource.t(), (-> term), nil | pos_integer()) ::
          {:ok, term} | {:error, term}
  def run_transaction(data_layer_module, resource, callback, timeout) do
    result = apply(data_layer_module, :transaction, [resource, callback, timeout])

    if match?({:ok, _}, result) or match?({:error, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.transaction/3.
        The callback #{inspect(__MODULE__)}.transaction/4 expects {:ok, term} or {:error, term}.
        """
    end
  end

  @doc false
  @spec run_transaction(
          module(),
          Ash.Resource.t(),
          (-> term),
          nil | pos_integer(),
          transaction_reason()
        ) ::
          {:ok, term} | {:error, term}
  def run_transaction(data_layer_module, resource, callback, timeout, reason) do
    result = apply(data_layer_module, :transaction, [resource, callback, timeout, reason])

    if match?({:ok, _}, result) or match?({:error, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.transaction/4.
        The callback #{inspect(__MODULE__)}.transaction/4 expects {:ok, term} or {:error, term}.
        """
    end
  end

  @doc "Rolls back the current transaction"
  @spec rollback(Ash.Resource.t() | list(Ash.Resource.t()), term) :: no_return
  def rollback([resource | _], term) do
    rollback(resource, term)
  end

  def rollback(resource, term) do
    rollback(Ash.DataLayer.data_layer(resource), resource, term)
  end

  @doc false
  @spec rollback(module(), Ash.Resource.t(), term) :: no_return
  def rollback(data_layer_module, resource, term) do
    apply(data_layer_module, :rollback, [resource, term])
  end

  @spec resource_to_query(Ash.Resource.t(), Ash.Domain.t()) :: data_layer_query()
  def resource_to_query(resource, domain) do
    resource_to_query(Ash.DataLayer.data_layer(resource), resource, domain)
  end

  @doc false
  @spec resource_to_query(module(), Ash.Resource.t(), Ash.Domain.t()) :: data_layer_query()
  def resource_to_query(data_layer_module, resource, domain) do
    apply(data_layer_module, :resource_to_query, [resource, domain])
  end

  @spec update(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def update(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}
    update(Ash.DataLayer.data_layer(resource), resource, changeset)
  end

  @doc false
  @spec update(module(), Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def update(data_layer_module, resource, changeset) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :update,
      [resource, changeset],
      [{:ok, :_}, {:error, :_}, {:error, :no_rollback, :_}],
      behaviour: __MODULE__,
      callback_name: "update/2"
    )
  end

  @spec update_query(data_layer_query(), Ash.Changeset.t(), opts :: bulk_update_options()) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def update_query(query, changeset, opts) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    update_query(
      Ash.DataLayer.data_layer(changeset.resource),
      query,
      changeset,
      changeset.resource,
      opts
    )
  end

  @doc false
  @spec update_query(
          module(),
          data_layer_query(),
          Ash.Changeset.t(),
          Ash.Resource.t(),
          bulk_update_options()
        ) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def update_query(data_layer_module, query, changeset, resource, opts) do
    result = apply(data_layer_module, :update_query, [query, changeset, resource, opts])

    if match?(:ok, result) or match?({:ok, _}, result) or match?({:error, _}, result) or
         match?({:error, :no_rollback, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.update_query/4.
        The callback #{inspect(__MODULE__)}.update_query/4 expects :ok, {:ok, enumerable}, {:error, term}, or {:error, :no_rollback, term}.
        """
    end
  end

  @spec destroy_query(data_layer_query(), Ash.Changeset.t(), opts :: bulk_update_options()) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def destroy_query(query, changeset, opts) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    destroy_query(
      Ash.DataLayer.data_layer(changeset.resource),
      query,
      changeset,
      changeset.resource,
      opts
    )
  end

  @doc false
  @spec destroy_query(
          module(),
          data_layer_query(),
          Ash.Changeset.t(),
          Ash.Resource.t(),
          bulk_update_options()
        ) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def destroy_query(data_layer_module, query, changeset, resource, opts) do
    result = apply(data_layer_module, :destroy_query, [query, changeset, resource, opts])

    if match?(:ok, result) or match?({:ok, _}, result) or match?({:error, _}, result) or
         match?({:error, :no_rollback, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.destroy_query/4.
        The callback #{inspect(__MODULE__)}.destroy_query/4 expects :ok, {:ok, enumerable}, {:error, term}, or {:error, :no_rollback, term}.
        """
    end
  end

  @spec create(Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def create(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}
    create(Ash.DataLayer.data_layer(resource), resource, changeset)
  end

  @doc false
  @spec create(module(), Ash.Resource.t(), Ash.Changeset.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term} | {:error, :no_rollback, term}
  def create(data_layer_module, resource, changeset) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :create,
      [resource, changeset],
      [{:ok, :_}, {:error, :_}, {:error, :no_rollback, :_}],
      behaviour: __MODULE__,
      callback_name: "create/2"
    )
  end

  @spec return_query(data_layer_query(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def return_query(query, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :return_query, 2) do
      return_query(data_layer, query, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec return_query(module(), data_layer_query(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def return_query(data_layer_module, query, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :return_query,
      [query, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "return_query/2"
    )
  end

  @spec bulk_create(
          Ash.Resource.t(),
          Enumerable.t(Ash.Changeset.t()),
          options :: bulk_create_options
        ) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:partial_success,
             failed :: Enumerable.t({error :: term(), changeset :: Ash.Changeset.t()}),
             Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def bulk_create(resource, changesets, options) do
    bulk_create(Ash.DataLayer.data_layer(resource), resource, changesets, options)
  end

  @doc false
  @spec bulk_create(
          module(),
          Ash.Resource.t(),
          Enumerable.t(Ash.Changeset.t()),
          bulk_create_options()
        ) ::
          :ok
          | {:ok, Enumerable.t(Ash.Resource.record())}
          | {:partial_success,
             failed :: Enumerable.t({error :: term(), changeset :: Ash.Changeset.t()}),
             Enumerable.t(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
          | {:error, :no_rollback, Ash.Error.t()}
  def bulk_create(data_layer_module, resource, changesets, options) do
    result = apply(data_layer_module, :bulk_create, [resource, changesets, options])

    if match?(:ok, result) or match?({:ok, _}, result) or match?({:partial_success, _, _}, result) or
         match?({:error, _}, result) or match?({:error, :no_rollback, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.bulk_create/3.
        The callback #{inspect(__MODULE__)}.bulk_create/3 expects :ok, {:ok, enumerable}, {:partial_success, failed, records}, {:error, term}, or {:error, :no_rollback, term}.
        """
    end
  end

  @spec destroy(Ash.Resource.t(), Ash.Changeset.t()) ::
          :ok | {:error, term} | {:error, :no_rollback, term}
  def destroy(resource, changeset) do
    changeset = %{changeset | tenant: changeset.to_tenant}

    destroy(Ash.DataLayer.data_layer(resource), resource, changeset)
  end

  @doc false
  @spec destroy(module(), Ash.Resource.t(), Ash.Changeset.t()) ::
          :ok | {:error, term} | {:error, :no_rollback, term}
  def destroy(data_layer_module, resource, changeset) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :destroy,
      [resource, changeset],
      [:ok, {:error, :_}, {:error, :no_rollback, :_}],
      behaviour: __MODULE__,
      callback_name: "destroy/2"
    )
  end

  @spec source(Ash.Resource.t()) :: String.t()
  def source(resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    Code.ensure_compiled!(data_layer)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :source, 1) do
      source(data_layer, resource)
    else
      ""
    end
  end

  @spec set_tenant(Ash.Resource.t(), data_layer_query(), String.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_tenant(resource, query, term) do
    set_tenant(Ash.DataLayer.data_layer(resource), resource, query, term)
  end

  @doc false
  @spec set_tenant(module(), Ash.Resource.t(), data_layer_query(), term()) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_tenant(data_layer_module, resource, query, term) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :set_tenant,
      [resource, query, term],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "set_tenant/3"
    )
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

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :upsert, 4) do
      run_upsert(data_layer, resource, changeset, keys, identity)
    else
      run_upsert(data_layer, resource, changeset, keys)
    end
  end

  @doc false
  @spec run_upsert(module(), Ash.Resource.t(), Ash.Changeset.t(), list(atom)) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record() | {:upsert_skipped, Ash.Query.t(), (-> term)}}
          | {:error, term}
          | {:error, :no_rollback, term}
  def run_upsert(data_layer_module, resource, changeset, keys) do
    result = apply(data_layer_module, :upsert, [resource, changeset, keys])

    if match?({:ok, _}, result) or match?({:error, _}, result) or
         match?({:error, :no_rollback, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.upsert/3.
        The callback #{inspect(__MODULE__)}.upsert/3 expects {:ok, term}, {:error, term}, or {:error, :no_rollback, term}.
        """
    end
  end

  @doc false
  @spec run_upsert(
          module(),
          Ash.Resource.t(),
          Ash.Changeset.t(),
          list(atom),
          Ash.Resource.Identity.t() | nil
        ) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record() | {:upsert_skipped, Ash.Query.t(), (-> term)}}
          | {:error, term}
          | {:error, :no_rollback, term}
  def run_upsert(data_layer_module, resource, changeset, keys, identity) do
    result = apply(data_layer_module, :upsert, [resource, changeset, keys, identity])

    if match?({:ok, _}, result) or match?({:error, _}, result) or
         match?({:error, :no_rollback, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.upsert/4.
        The callback #{inspect(__MODULE__)}.upsert/4 expects {:ok, term}, {:error, term}, or {:error, :no_rollback, term}.
        """
    end
  end

  @spec set_context(Ash.Resource.t(), data_layer_query(), map) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_context(resource, query, map) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :set_context, 3) do
      set_context(data_layer, resource, query, map)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec set_context(module(), Ash.Resource.t(), data_layer_query(), map()) ::
          {:ok, data_layer_query()} | {:error, term}
  def set_context(data_layer_module, resource, query, map) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :set_context,
      [resource, query, map],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "set_context/3"
    )
  end

  @spec filter(data_layer_query(), Ash.Filter.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def filter(query, nil, _), do: {:ok, query}

  def filter(query, filter, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if can?(data_layer, resource, :filter) do
      if can?(data_layer, resource, :boolean_filter) do
        filter(data_layer, query, filter, resource)
      else
        simple_filter = Ash.Filter.to_simple_filter(filter)
        filter(data_layer, query, simple_filter, resource)
      end
    else
      {:error, "Data layer does not support filtering"}
    end
  end

  @doc false
  @spec filter(module(), data_layer_query(), Ash.Filter.t() | term(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def filter(data_layer_module, query, filter, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :filter,
      [query, filter, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "filter/3"
    )
  end

  @spec combination_of(
          [{combination_type, data_layer_query}],
          resource :: Ash.Resource.t(),
          domain :: Ash.Domain.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def combination_of(combinations, resource, domain) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if can?(data_layer, resource, :combine) do
      combinations
      |> Enum.map(&elem(&1, 0))
      |> Enum.uniq()
      |> Enum.find(fn type ->
        !can?(data_layer, resource, {:combine, type})
      end)
      |> case do
        nil ->
          combination_of(data_layer, combinations, resource, domain)

        type ->
          {:error, "Data layer does not support combining queries with `#{inspect(type)}`"}
      end
    else
      {:error, "Data layer does not support combining queries"}
    end
  end

  @doc false
  @spec combination_of(
          module(),
          [{combination_type, data_layer_query()}],
          Ash.Resource.t(),
          Ash.Domain.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def combination_of(data_layer_module, combinations, resource, domain) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :combination_of,
      [combinations, resource, domain],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "combination_of/3"
    )
  end

  @spec combination_acc(data_layer_query(), Ash.Resource.t()) :: any()
  def combination_acc(data_layer_query, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if function_exported?(data_layer, :combination_acc, 1) do
      run_combination_acc(data_layer, data_layer_query)
    else
      data_layer
    end
  end

  @doc false
  @spec run_combination_acc(module(), data_layer_query()) :: any()
  def run_combination_acc(data_layer_module, data_layer_query) do
    apply(data_layer_module, :combination_acc, [data_layer_query])
  end

  @spec sort(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def sort(query, sort, resource) do
    if can?(:sort, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      sort(data_layer, query, sort, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec sort(module(), data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def sort(data_layer_module, query, sort, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :sort,
      [query, sort, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "sort/3"
    )
  end

  @spec distinct(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct(query, distinct, resource) do
    if can?(:distinct, resource) && distinct do
      data_layer = Ash.DataLayer.data_layer(resource)
      distinct(data_layer, query, distinct, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec distinct(module(), data_layer_query(), list(atom), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct(data_layer_module, query, distinct, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :distinct,
      [query, distinct, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "distinct/3"
    )
  end

  @spec distinct_sort(data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct_sort(query, sort, resource) do
    if can?(:distinct_sort, resource) && sort do
      data_layer = Ash.DataLayer.data_layer(resource)
      distinct_sort(data_layer, query, sort, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec distinct_sort(module(), data_layer_query(), Ash.Sort.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def distinct_sort(data_layer_module, query, sort, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :distinct_sort,
      [query, sort, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "distinct_sort/3"
    )
  end

  @spec limit(data_layer_query(), limit :: non_neg_integer, Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def limit(query, nil, _resource), do: {:ok, query}

  def limit(query, limit, resource) do
    if can?(:limit, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      limit(data_layer, query, limit, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec limit(module(), data_layer_query(), non_neg_integer(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def limit(data_layer_module, query, limit, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :limit,
      [query, limit, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "limit/3"
    )
  end

  @spec offset(data_layer_query(), offset :: non_neg_integer, Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def offset(query, nil, _resource), do: {:ok, query}

  def offset(query, offset, resource) do
    if can?(:offset, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      offset(data_layer, query, offset, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec offset(module(), data_layer_query(), non_neg_integer(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def offset(data_layer_module, query, offset, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :offset,
      [query, offset, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "offset/3"
    )
  end

  @spec select(data_layer_query(), select :: list(atom), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def select(query, select, resource) do
    if can?(:select, resource) do
      select =
        select || Enum.to_list(Ash.Resource.Info.selected_by_default_attribute_names(resource))

      data_layer = Ash.DataLayer.data_layer(resource)
      select(data_layer, query, select, resource)
    else
      {:ok, query}
    end
  end

  @doc false
  @spec select(module(), data_layer_query(), list(atom), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def select(data_layer_module, query, select, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :select,
      [query, select, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "select/3"
    )
  end

  @spec add_aggregates(data_layer_query(), list(Ash.Query.Aggregate.t()), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_aggregates(query, [], _) do
    {:ok, query}
  end

  def add_aggregates(query, aggregates, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :add_aggregates, 3) do
      add_aggregates(data_layer, query, aggregates, resource)
    else
      Enum.reduce_while(aggregates, {:ok, query}, fn aggregate, {:ok, data_layer_query} ->
        case add_aggregate(data_layer, data_layer_query, aggregate, resource) do
          {:ok, data_layer_query} -> {:cont, {:ok, data_layer_query}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)
    end
  end

  @doc false
  @spec add_aggregates(
          module(),
          data_layer_query(),
          list(Ash.Query.Aggregate.t()),
          Ash.Resource.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_aggregates(data_layer_module, query, aggregates, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :add_aggregates,
      [query, aggregates, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "add_aggregates/3"
    )
  end

  @doc false
  @spec add_aggregate(module(), data_layer_query(), Ash.Query.Aggregate.t(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_aggregate(data_layer_module, query, aggregate, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :add_aggregate,
      [query, aggregate, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "add_aggregate/3"
    )
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

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :add_calculations, 3) do
      add_calculations(data_layer, query, calculations, resource)
    else
      Enum.reduce_while(calculations, {:ok, query}, fn {calculation, expression}, {:ok, query} ->
        case add_calculation(data_layer, query, calculation, expression, resource) do
          {:ok, query} -> {:cont, {:ok, query}}
          {:error, error} -> {:halt, {:error, error}}
        end
      end)
    end
  end

  @doc false
  @spec add_calculations(
          module(),
          data_layer_query(),
          list({Ash.Query.Calculation.t(), term()}),
          Ash.Resource.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_calculations(data_layer_module, query, calculations, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :add_calculations,
      [query, calculations, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "add_calculations/3"
    )
  end

  @doc false
  @spec add_calculation(
          module(),
          data_layer_query(),
          Ash.Query.Calculation.t(),
          term(),
          Ash.Resource.t()
        ) ::
          {:ok, data_layer_query()} | {:error, term}
  def add_calculation(data_layer_module, query, calculation, expression, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :add_calculation,
      [query, calculation, expression, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "add_calculation/4"
    )
  end

  @spec can?(feature, Ash.Resource.t() | Spark.Dsl.t()) :: boolean
  def can?(feature, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)
    can?(data_layer, resource, feature)
  end

  @doc false
  @spec can?(module(), Ash.Resource.t() | Spark.Dsl.t(), feature()) :: boolean
  def can?(data_layer_module, resource, feature) do
    result = apply(data_layer_module, :can?, [resource, feature])

    if is_boolean(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.can?/2.
        The callback #{inspect(__MODULE__)}.can?/2 expects a boolean.
        """
    end
  end

  @spec run_aggregate_query(
          data_layer_query(),
          list(Ash.Query.Aggregate.t()),
          Ash.Resource.t()
        ) ::
          {:ok, map} | {:error, term}
  def run_aggregate_query(query, aggregates, resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :run_aggregate_query, 3) do
      run_aggregate_query(data_layer, query, aggregates, resource)
    else
      {:error, "Aggregate queries not supported by #{inspect(data_layer)}"}
    end
  end

  @doc false
  @spec run_aggregate_query(
          module(),
          data_layer_query(),
          list(Ash.Query.Aggregate.t()),
          Ash.Resource.t()
        ) ::
          {:ok, map()} | {:error, term}
  def run_aggregate_query(data_layer_module, query, aggregates, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :run_aggregate_query,
      [query, aggregates, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "run_aggregate_query/3"
    )
  end

  @spec run_query(data_layer_query(), central_resource :: Ash.Resource.t()) ::
          {:ok, list(Ash.Resource.record())} | {:error, term} | {:error, :no_rollback, term}
  def run_query(query, central_resource) do
    run_query(Ash.DataLayer.data_layer(central_resource), query, central_resource)
  end

  @doc false
  @spec run_query(module(), data_layer_query(), Ash.Resource.t()) ::
          {:ok, list(Ash.Resource.record())} | {:error, term} | {:error, :no_rollback, term}
  def run_query(data_layer_module, query, central_resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :run_query,
      [query, central_resource],
      [{:ok, :_}, {:error, :_}, {:error, :no_rollback, :_}],
      behaviour: __MODULE__,
      callback_name: "run_query/2"
    )
  end

  @spec lock(data_layer_query(), lock_type :: lock_type() | nil, resource :: Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def lock(query, nil, _), do: {:ok, query}

  def lock(query, lock_type, resource) do
    lock(Ash.DataLayer.data_layer(resource), query, lock_type, resource)
  end

  @doc false
  @spec lock(module(), data_layer_query(), lock_type(), Ash.Resource.t()) ::
          {:ok, data_layer_query()} | {:error, term}
  def lock(data_layer_module, query, lock_type, resource) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :lock,
      [query, lock_type, resource],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "lock/3"
    )
  end

  def run_aggregate_query_with_lateral_join(
        query,
        aggregates,
        root_data,
        destination_resource,
        path
      ) do
    run_aggregate_query_with_lateral_join(
      Ash.DataLayer.data_layer(destination_resource),
      query,
      aggregates,
      root_data,
      destination_resource,
      path
    )
  end

  @doc false
  @spec run_aggregate_query_with_lateral_join(
          module(),
          data_layer_query(),
          list(Ash.Query.Aggregate.t()),
          [Ash.Resource.record()],
          Ash.Resource.t(),
          list(lateral_join_link())
        ) :: {:ok, list(Ash.Resource.t())} | {:error, term}
  def run_aggregate_query_with_lateral_join(
        data_layer_module,
        query,
        aggregates,
        root_data,
        destination_resource,
        path
      ) do
    result =
      apply(data_layer_module, :run_aggregate_query_with_lateral_join, [
        query,
        aggregates,
        root_data,
        destination_resource,
        path
      ])

    if match?({:ok, _}, result) or match?({:error, _}, result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.run_aggregate_query_with_lateral_join/5.
        The callback #{inspect(__MODULE__)}.run_aggregate_query_with_lateral_join/5 expects {:ok, list} or {:error, term}.
        """
    end
  end

  def run_query_with_lateral_join(
        query,
        root_data,
        destination_resource,
        path
      ) do
    run_query_with_lateral_join(
      Ash.DataLayer.data_layer(destination_resource),
      query,
      root_data,
      destination_resource,
      path
    )
  end

  @doc false
  @spec run_query_with_lateral_join(
          module(),
          data_layer_query(),
          [Ash.Resource.record()],
          Ash.Resource.t(),
          list(lateral_join_link())
        ) :: {:ok, list(Ash.Resource.record())} | {:error, term}
  def run_query_with_lateral_join(
        data_layer_module,
        query,
        root_data,
        destination_resource,
        path
      ) do
    Ash.BehaviourHelpers.call_and_validate_return(
      data_layer_module,
      :run_query_with_lateral_join,
      [query, root_data, destination_resource, path],
      [{:ok, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "run_query_with_lateral_join/4"
    )
  end

  def in_transaction?(resource) do
    if can?(:transact, resource) do
      data_layer = Ash.DataLayer.data_layer(resource)
      in_transaction?(data_layer, resource)
    else
      false
    end
  end

  @doc false
  @spec in_transaction?(module(), Ash.Resource.t()) :: boolean
  def in_transaction?(data_layer_module, resource) do
    result = apply(data_layer_module, :in_transaction?, [resource])

    if is_boolean(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.in_transaction?/1.
        The callback #{inspect(__MODULE__)}.in_transaction?/1 expects a boolean.
        """
    end
  end

  def functions(resource) do
    data_layer = Ash.DataLayer.data_layer(resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :functions, 1) do
      functions(data_layer, resource)
    else
      %{}
    end
  end

  @doc false
  @spec functions(module(), Ash.Resource.t()) :: [module()]
  def functions(data_layer_module, resource) do
    result = apply(data_layer_module, :functions, [resource])

    if is_list(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.functions/1.
        The callback #{inspect(__MODULE__)}.functions/1 expects a list of modules.
        """
    end
  end

  def transform_query(query) do
    data_layer = Ash.DataLayer.data_layer(query.resource)

    if Code.ensure_loaded?(data_layer) && function_exported?(data_layer, :transform_query, 1) do
      transform_query(data_layer, query)
    else
      query
    end
  end

  @doc false
  @spec transform_query(module(), Ash.Query.t()) :: Ash.Query.t()
  def transform_query(data_layer_module, query) do
    result = apply(data_layer_module, :transform_query, [query])

    if is_struct(result, Ash.Query) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.transform_query/1.
        The callback #{inspect(__MODULE__)}.transform_query/1 expects an Ash.Query.t().
        """
    end
  end

  @doc false
  @spec source(module(), Ash.Resource.t()) :: String.t()
  def source(data_layer_module, resource) do
    result = apply(data_layer_module, :source, [resource])

    if is_binary(result) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(data_layer_module)}.source/1.
        The callback #{inspect(__MODULE__)}.source/1 expects a String.t().
        """
    end
  end
end
