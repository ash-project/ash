# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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
      transaction_group: [
        type: :atom,
        doc:
          "Runs the resource's actions in transactions shared with every resource that names the same group, as resources sharing a repo share a transaction. A transaction's writes reach the tables when it commits, so other processes read only what was committed, and a rollback through any resource of the group leaves the tables as they were. Reads take no locks. A write locks the record it writes, and the identity values it takes, until its transaction ends, waiting for a lock another transaction holds and failing only when its wait would deadlock, and a write made outside any transaction runs in one of its own. The resource's identities are enforced when it writes, so they need no `pre_check_with`. A value a destroy or an update gives up stays taken until its transaction commits, so a create of the value meanwhile is refused, as Ash's identity check refuses a value a committed record holds. A transaction's timeout is checked at each table it reads or writes and when it commits."
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

  Remember, this has no support for transactions unless the resource names a
  `transaction_group` in its `ets` section. This is not recommended for production use,
  especially in multi-user applications. It can, however, be great for prototyping.
  """

  use Spark.Dsl.Extension,
    sections: [@ets],
    verifiers: [Ash.DataLayer.Ets.Verifiers.RequirePreCheckWith]

  alias Ash.Actions.Sort
  alias Ash.Error.Invalid.InvalidPrimaryKey

  defmodule Query do
    @moduledoc false
    defstruct [
      :resource,
      :filter,
      :limit,
      :tenant,
      :as_of,
      :domain,
      :select,
      sort: [],
      distinct: [],
      distinct_sort: nil,
      combination_of: [],
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

    def start(resource, tenant, create? \\ true) do
      case Ash.DataLayer.Ets.table_name(resource, tenant, create?) do
        :no_table ->
          :no_table

        {:ok, table} ->
          start_table(resource, table, create?)
      end
    end

    defp start_table(_resource, table, false) do
      case ETS.Set.wrap_existing(table) do
        {:error, :table_not_found} -> :no_table
        other -> other
      end
    end

    # sobelow_skip ["DOS.ModuleConcat"]
    defp start_table(resource, table, true) do
      if Ash.DataLayer.Ets.Info.private?(resource) do
        do_wrap_existing(resource, table)
      else
        case GenServer.start(__MODULE__, {resource, table},
               name: Module.concat(table, TableManager)
             ) do
          {:error, {:already_started, pid}} ->
            await_init(pid)
            ETS.Set.wrap_existing(table)

          {:error, error} ->
            {:error, error}

          _ ->
            ETS.Set.wrap_existing(table)
        end
      end
    end

    # A manager's name is registered before its init/1 creates the table, and a call is
    # only answered once init/1 has returned. A manager that died answers nothing; the
    # table lookup that follows then reports the table as missing.
    defp await_init(pid) do
      GenServer.call(pid, :wait)
    catch
      :exit, _ -> :ok
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

  defmodule LockManager do
    @moduledoc false
    use GenServer

    # The locks of the transactions writing shared tables, held in one process that sees every
    # wait. A lock names a table, by its name and tenant, and either one item of it (a record's
    # primary key or an identity's value) or the whole table. An item conflicts with itself and
    # with the whole table, which conflicts with every item. A transaction is granted what it
    # holds already, and every item of a table it holds whole. A request is granted when it
    # conflicts with nothing another transaction holds or asked for before it, so conflicting
    # requests are granted in the order they arrive, each waiting for as long as its transaction
    # may run, and one is refused only when its wait would deadlock.
    def lock(table, item, id, timeout) do
      manager =
        case GenServer.start(__MODULE__, nil, name: __MODULE__) do
          {:ok, pid} -> pid
          {:error, {:already_started, pid}} -> pid
        end

      GenServer.call(manager, {:lock, table, item, id}, timeout)
    catch
      :exit, {:timeout, _} -> :timeout
    end

    # One lock of a transaction goes, held or asked for; the others stay until it ends.
    def unlock(table, item, id) do
      case Process.whereis(__MODULE__) do
        nil -> :ok
        pid -> GenServer.call(pid, {:unlock, table, item, id})
      end
    catch
      :exit, _ -> :ok
    end

    # The processes whose transactions, other than the given one, hold a lock of a table.
    def holders(table, id) do
      case Process.whereis(__MODULE__) do
        nil -> []
        pid -> GenServer.call(pid, {:holders, table, id})
      end
    catch
      :exit, _ -> []
    end

    # A transaction's locks and waiting request all go when it ends.
    def release(id) do
      case Process.whereis(__MODULE__) do
        nil -> :ok
        pid -> GenServer.call(pid, {:release, id})
      end
    catch
      :exit, _ -> :ok
    end

    def init(nil), do: {:ok, %{tables: %{}, transactions: %{}}}

    def handle_call({:lock, table, item, id}, {pid, _} = from, state) do
      state = track(state, id, pid, table)
      entry = Map.get(state.tables, table, %{held: %{}, queue: []})

      case blockers(entry, id, item, entry.queue) do
        [] ->
          {:reply, :ok, put_entry(state, table, %{entry | held: Map.put(entry.held, item, id)})}

        # Waiting closes a cycle when a transaction this one would wait for already waits,
        # through the transactions it waits for, on this one. The BEAM detects no such cycle
        # among waiting processes, so this is PostgreSQL's deadlock detection: the request that
        # would close a cycle of waits is refused
        # (https://github.com/postgres/postgres/blob/master/src/backend/storage/lmgr/README).
        blockers ->
          if waits_on?(state, blockers, id, MapSet.new()) do
            {:reply, :deadlock, state}
          else
            entry = %{entry | queue: entry.queue ++ [{from, id, item}]}
            {:noreply, state |> put_entry(table, entry) |> put_waiting(id, table)}
          end
      end
    end

    def handle_call({:unlock, table, item, id}, _from, state) do
      case state.tables do
        %{^table => entry} ->
          {asked, queue} = Enum.split_with(entry.queue, &match?({_from, ^id, ^item}, &1))
          held = if entry.held[item] == id, do: Map.delete(entry.held, item), else: entry.held
          state = if asked == [], do: state, else: put_waiting(state, id, nil)
          {:reply, :ok, grant(state, table, %{entry | held: held, queue: queue})}

        _ ->
          {:reply, :ok, state}
      end
    end

    def handle_call({:holders, table, id}, _from, state) do
      held = Map.get(state.tables, table, %{held: %{}}).held

      {:reply,
       for({_item, holder} <- held, holder != id, uniq: true, do: state.transactions[holder].pid),
       state}
    end

    def handle_call({:release, id}, _from, state), do: {:reply, :ok, release(state, id)}

    def handle_info({:DOWN, monitor, :process, _, _}, state) do
      case Enum.find(state.transactions, fn {_id, transaction} ->
             transaction.monitor == monitor
           end) do
        {id, _transaction} -> {:noreply, release(state, id)}
        nil -> {:noreply, state}
      end
    end

    def handle_info(_, state), do: {:noreply, state}

    defp release(state, id) do
      case Map.pop(state.transactions, id) do
        {nil, _transactions} ->
          state

        {%{monitor: monitor, tables: tables}, transactions} ->
          Process.demonitor(monitor, [:flush])

          Enum.reduce(tables, %{state | transactions: transactions}, fn table, state ->
            case state.tables do
              %{^table => entry} ->
                grant(state, table, %{
                  held: Map.reject(entry.held, fn {_item, holder} -> holder == id end),
                  queue: Enum.reject(entry.queue, fn {_from, waiter, _item} -> waiter == id end)
                })

              _ ->
                state
            end
          end)
      end
    end

    # Grants, in the order they arrived, the waiting requests that conflict with nothing held
    # or asked for before them.
    defp grant(state, table, entry) do
      {entry, granted} =
        Enum.reduce(entry.queue, {%{entry | queue: []}, []}, fn {from, waiter, item} = request,
                                                                {entry, granted} ->
          case blockers(entry, waiter, item, entry.queue) do
            [] -> {%{entry | held: Map.put(entry.held, item, waiter)}, [{from, waiter} | granted]}
            _ -> {%{entry | queue: entry.queue ++ [request]}, granted}
          end
        end)

      Enum.reduce(granted, put_entry(state, table, entry), fn {from, waiter}, state ->
        GenServer.reply(from, :ok)
        put_waiting(state, waiter, nil)
      end)
    end

    # The transactions a request waits for: those holding a lock it conflicts with, and those
    # asking before it for one.
    defp blockers(entry, id, item, ahead) do
      if entry.held[:table] == id or entry.held[item] == id do
        []
      else
        held =
          for {other, holder} <- entry.held, holder != id, conflicts?(item, other), do: holder

        asked =
          for {_from, waiter, other} <- ahead, waiter != id, conflicts?(item, other), do: waiter

        Enum.uniq(held ++ asked)
      end
    end

    defp conflicts?(:table, _item), do: true
    defp conflicts?(_item, :table), do: true
    defp conflicts?(item, other), do: item == other

    defp waits_on?(state, transactions, id, seen) do
      Enum.any?(transactions, fn
        ^id ->
          true

        other ->
          with false <- MapSet.member?(seen, other),
               %{waiting: table} when not is_nil(table) <- state.transactions[other] do
            entry = state.tables[table]

            {ahead, [{_from, ^other, item} | _]} =
              Enum.split_while(entry.queue, fn {_from, waiter, _item} -> waiter != other end)

            waits_on?(state, blockers(entry, other, item, ahead), id, MapSet.put(seen, other))
          else
            _ -> false
          end
      end)
    end

    defp track(state, id, pid, table) do
      transactions =
        state.transactions
        |> Map.put_new_lazy(id, fn ->
          %{pid: pid, monitor: Process.monitor(pid), tables: MapSet.new(), waiting: nil}
        end)
        |> Map.update!(id, &%{&1 | tables: MapSet.put(&1.tables, table)})

      %{state | transactions: transactions}
    end

    defp put_entry(state, table, %{held: held, queue: []}) when held == %{},
      do: %{state | tables: Map.delete(state.tables, table)}

    defp put_entry(state, table, entry),
      do: %{state | tables: Map.put(state.tables, table, entry)}

    defp put_waiting(state, id, table),
      do: %{state | transactions: Map.update!(state.transactions, id, &%{&1 | waiting: table})}
  end

  @doc false
  # Tenant values are routinely derived from user input, and atoms are never garbage
  # collected, so we only intern a new table name when we are actually going to create
  # the table. Otherwise we look the atom up, and treat a name that was never interned
  # the same as a table that doesn't exist.
  # sobelow_skip ["DOS.StringToAtom", "DOS.ModuleConcat"]
  def table_name(resource, tenant, create?) do
    table = Ash.DataLayer.Ets.Info.table(resource)

    if tenant && Ash.Resource.Info.multitenancy_strategy(resource) == :context do
      if create? do
        {:ok, Module.concat(to_string(table), to_string(tenant))}
      else
        try do
          {:ok, Module.safe_concat(to_string(table), to_string(tenant))}
        rescue
          ArgumentError -> :no_table
        end
      end
    else
      {:ok, table}
    end
  end

  @stop_opts [
    timeout: [
      type: :timeout,
      default: :infinity,
      doc: "How long, in milliseconds, to wait for the transactions holding the table."
    ]
  ]

  @doc """
  Stops the storage for a given resource/tenant (deleting all of the data).

  For a resource in a transaction group, the table is deleted once the transactions of other
  processes holding any of its locks have ended, and writes asking for it meanwhile wait until
  it is gone. The wait lasts as long as those transactions run, unless `:timeout` bounds it.

  Returns `:ok`, or raises: past the timeout, naming the processes whose transactions hold the
  table, and when one of those transactions waits for the caller's own transaction, since the
  wait would never end.

  ## Options

  #{Spark.Options.docs(@stop_opts)}
  """
  # sobelow_skip ["DOS.ModuleConcat"]
  def stop(resource, tenant \\ nil, opts \\ []) do
    opts = Spark.Options.validate!(opts, @stop_opts)

    tenant =
      if Ash.Resource.Info.multitenancy_strategy(resource) == :context do
        tenant
      end

    if Ash.DataLayer.Ets.Info.private?(resource) do
      case Process.get({:ash_ets_table, Ash.DataLayer.Ets.Info.table(resource), tenant}) do
        nil ->
          :ok

        table ->
          ETS.Set.delete(table)
      end
    else
      with {:ok, table} <- table_name(resource, tenant, false),
           pid when is_pid(pid) <- Process.whereis(Module.concat(table, TableManager)) do
        if Ash.DataLayer.Ets.Info.transaction_group(resource) do
          stop_locked(resource, tenant, pid, opts[:timeout])
        else
          stop_manager(pid)
        end
      else
        _no_table -> :ok
      end
    end
  end

  # A table is stopped under a lock on the whole of it, asked for in turn like any other, so
  # the transactions holding any of its locks end and commit first, and the locks asked for
  # after it wait until the table is gone. The caller's own transaction never waits on itself,
  # and keeps its other locks. A stop whose wait times out gives up its request, so nothing
  # waits behind it, and one whose wait would deadlock is never queued.
  defp stop_locked(resource, tenant, pid, timeout) do
    group = Ash.DataLayer.Ets.Info.transaction_group(resource)
    key = lock_key(resource, tenant)

    {id, own?} =
      case Process.get({:ash_ets_transaction, group}) do
        %{id: id} -> {id, true}
        nil -> {System.unique_integer([:monotonic, :positive]), false}
      end

    release = fn ->
      if own?, do: LockManager.unlock(key, :table, id), else: LockManager.release(id)
    end

    case LockManager.lock(key, :table, id, timeout) do
      :ok ->
        stop_manager(pid)
        release.()
        :ok

      :deadlock ->
        raise "the table of #{inspect(resource)} was not stopped: " <>
                "the transactions holding it wait for the caller's own transaction"

      :timeout ->
        holders = LockManager.holders(key, id)
        release.()

        raise "the table of #{inspect(resource)} was not stopped within #{timeout}ms: " <>
                "the transactions of #{inspect(holders)} hold it"
    end
  end

  # Waits for the manager to exit, so its table is gone by the time this returns.
  defp stop_manager(pid) do
    ref = Process.monitor(pid)
    Process.exit(pid, :shutdown)

    receive do
      {:DOWN, ^ref, :process, _, _} -> :ok
    end
  end

  @doc false
  @impl true
  def can?(_, :distinct_sort), do: true
  def can?(resource, :async_engine), do: not Ash.DataLayer.Ets.Info.private?(resource)
  def can?(_, {:lateral_join, _}), do: true
  def can?(_, :bulk_create), do: true
  def can?(_, :combine), do: true
  def can?(_, {:combine, _type}), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :temporal), do: true
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
  def can?(_, {:aggregate, :unrelated}), do: true
  def can?(_, {:exists, :unrelated}), do: true
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

  # A resource's actions take `transaction?` from this when the resource compiles.
  def can?(resource, :transact),
    do: not is_nil(Ash.DataLayer.Ets.Info.transaction_group(resource))

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
  def can?(_, :expr_error), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, {:atomic, :update}), do: true
  def can?(_, {:atomic, :upsert}), do: true
  def can?(_, {:atomic, :create}), do: true
  def can?(_, :through_relationship), do: true
  def can?(_, _), do: false

  # A transaction is its group's, as a database transaction is its repo's: every resource of
  # the group runs inside it. It reads committed data, as AshPostgres's transactions read in
  # PostgreSQL. Its writes wait in its store until it commits, as Mnesia keeps a transaction's
  # writes in its store
  # (https://github.com/erlang/otp/blob/master/lib/mnesia/src/mnesia.erl, write_to_store):
  # its own reads see them over the tables (add_written and add_match there), every other
  # process reads only what was committed, and a rollback or a timeout leaves the tables as
  # they were. Its reads take no locks. Each write locks what it writes, the record and the
  # identity values it takes, until the transaction ends; a lock another
  # transaction holds is waited for, and a transaction fails only when waiting would deadlock.
  # Its timeout is checked at each table it reads or writes and when it commits.
  @doc false
  @impl true
  def transaction(resource, func, timeout, reason) do
    group = Ash.DataLayer.Ets.Info.transaction_group(resource)

    transaction = %{
      id: System.unique_integer([:monotonic, :positive]),
      deadline: deadline(timeout),
      timeout: timeout,
      name: name(reason, resource)
    }

    Process.put({:ash_ets_transaction, group}, transaction)

    try do
      result = func.()
      store = Process.get({:ash_ets_store, group}, %{})

      cond do
        past?(transaction.deadline) ->
          {:error, timeout_error(transaction)}

        # A commit lands one table at a time, so it starts only once every table is there.
        Enum.any?(Map.keys(store), &(:ets.info(&1, :id) == :undefined)) ->
          {:error, "a table written in the transaction was stopped before it committed"}

        true ->
          for {tid, ops} <- store do
            for {key, :delete} <- ops, do: :ets.delete(tid, key)
            :ets.insert(tid, for({_key, {:write, object}} <- ops, do: object))
          end

          {:ok, result}
      end
    catch
      {{:ash_ets_rollback, ^group}, value} -> {:error, value}
    after
      release(group)
      Process.delete({:ash_ets_store, group})
      Process.delete({:ash_ets_transaction, group})
    end
  end

  @doc false
  @impl true
  def rollback(resource, value) do
    throw({{:ash_ets_rollback, Ash.DataLayer.Ets.Info.transaction_group(resource)}, value})
  end

  @doc false
  @impl true
  def in_transaction?(resource) do
    Process.get({:ash_ets_transaction, Ash.DataLayer.Ets.Info.transaction_group(resource)}) !=
      nil
  end

  # A write to a group's shared table made outside any transaction runs in one of its own, so
  # it waits for the transactions holding what it writes and lands before or after them. An
  # update or destroy of a tenant with no table has no transaction to wait for.
  defp transact_write(resource, tenant, create?, func) do
    if shared_table?(resource) and not is_nil(Ash.DataLayer.Ets.Info.transaction_group(resource)) and
         not in_transaction?(resource) and
         (create? or match?({:ok, _}, wrap_or_create_table(resource, tenant, false))) do
      case transaction(resource, func, :infinity, %{tenant: tenant}) do
        {:ok, result} -> result
        {:error, error} -> {:error, error}
      end
    else
      func.()
    end
  end

  # A table's locks name it by its name and, for a resource with context multitenancy, its
  # tenant, so locking a tenant's table creates nothing.
  defp lock_key(resource, tenant) do
    tenant =
      if Ash.Resource.Info.multitenancy_strategy(resource) == :context do
        tenant
      end

    {Ash.DataLayer.Ets.Info.table(resource), tenant}
  end

  # Locks one item of a table for the transaction open in this process, which holds it until
  # it ends. Refused for a deadlock or past its deadline, the transaction rolls back. A private
  # table, which no other process reaches, needs no lock.
  defp lock_item(resource, tenant, item) do
    check_deadline(resource)
    group = Ash.DataLayer.Ets.Info.transaction_group(resource)

    with %{} = transaction <- Process.get({:ash_ets_transaction, group}),
         false <- Ash.DataLayer.Ets.Info.private?(resource) do
      case LockManager.lock(
             lock_key(resource, tenant),
             item,
             transaction.id,
             remaining(transaction.deadline)
           ) do
        :ok -> :ok
        :timeout -> rollback(resource, timeout_error(transaction))
        :deadlock -> rollback(resource, deadlock_error(resource))
      end
    else
      _ -> :ok
    end
  end

  # A write locks the record it writes by its primary key, which a temporal record's versions
  # share. A record of a resource with no primary key is created under a key no other record
  # has, and is never written again.
  defp lock_record(resource, tenant, record) do
    if Ash.Resource.Info.primary_key(resource) != [],
      do: lock_item(resource, tenant, {:record, primary_key(resource, record)}),
      else: :ok
  end

  # The table a write writes, found once the write holds its locks, so a write that waited for
  # a stop finds the table the stop left. The open transaction's writes to it wait in its store.
  defp written_table(resource, tenant, create?) do
    group = Ash.DataLayer.Ets.Info.transaction_group(resource)

    with {:ok, table} <- wrap_or_create_table(resource, tenant, create?) do
      if Process.get({:ash_ets_transaction, group}), do: hold(table.table, group)
      {:ok, table}
    end
  end

  # Past its deadline, a transaction rolls back at the next table it reads or writes.
  defp check_deadline(resource) do
    case Process.get({:ash_ets_transaction, Ash.DataLayer.Ets.Info.transaction_group(resource)}) do
      %{deadline: deadline} = transaction ->
        if past?(deadline), do: rollback(resource, timeout_error(transaction)), else: :ok

      nil ->
        :ok
    end
  end

  defp hold(tid, group) do
    if !Process.put({:ash_ets_held, tid}, group) do
      Process.put({:ash_ets_held_tables, group}, [
        tid | Process.get({:ash_ets_held_tables, group}, [])
      ])
    end

    :ok
  end

  defp release(group) do
    for tid <- Process.get({:ash_ets_held_tables, group}, []),
        do: Process.delete({:ash_ets_held, tid})

    Process.delete({:ash_ets_held_tables, group})
    %{id: id} = Process.get({:ash_ets_transaction, group})
    LockManager.release(id)
  end

  defp deadlock_error(resource),
    do: "deadlock detected while waiting for a lock of #{inspect(resource)}"

  defp timeout_error(transaction),
    do: Ash.Error.Invalid.Timeout.exception(timeout: transaction.timeout, name: transaction.name)

  defp deadline(timeout) when is_integer(timeout),
    do: System.monotonic_time(:millisecond) + timeout

  defp deadline(_), do: :infinity

  defp remaining(:infinity), do: :infinity
  defp remaining(deadline), do: max(deadline - System.monotonic_time(:millisecond), 0)

  defp past?(:infinity), do: false
  defp past?(deadline), do: System.monotonic_time(:millisecond) > deadline

  defp name(%{metadata: %{resource: resource, action: action}}, _) when is_atom(action),
    do: "#{inspect(resource)}.#{action}"

  defp name(_, resource), do: inspect(resource)

  defp shared_table?(resource) do
    Ash.Resource.Info.data_layer(resource) == __MODULE__ and
      not Ash.DataLayer.Ets.Info.private?(resource)
  end

  # The writes of the open transaction that writes a table, waiting in its store until it
  # commits, which keeps them by table. A table no open transaction writes is read and written
  # directly.
  defp store_of(table) do
    with group when not is_nil(group) <- Process.get({:ash_ets_held, table.table}),
         do: {group, Map.get(Process.get({:ash_ets_store, group}, %{}), table.table, %{})}
  end

  defp write_to_store(table, group, ops) do
    store = Process.get({:ash_ets_store, group}, %{})
    Process.put({:ash_ets_store, group}, Map.update(store, table.table, ops, &Map.merge(&1, ops)))
  end

  # A transaction reads what it wrote over what the table holds, as Mnesia's add_written.
  defp get_object(table, key) do
    with {_group, ops} <- store_of(table),
         {:ok, op} <- Map.fetch(ops, key) do
      case op do
        {:write, object} -> {:ok, object}
        :delete -> {:ok, nil}
      end
    else
      _ -> ETS.Set.get(table, key)
    end
  end

  defp all_objects(table) do
    with {:ok, objects} <- ETS.Set.to_list(table), do: {:ok, merge(objects, table)}
  end

  # The match spec runs over what the transaction wrote too, as Mnesia's add_written_match.
  defp select_objects(table, spec) do
    case store_of(table) do
      {_group, ops} when ops != %{} ->
        heads = for {head, guards, _body} <- spec, do: {head, guards, [:"$_"]}

        with {:ok, objects} <- ETS.Set.select(table, heads) do
          {:ok, :ets.match_spec_run(merge(objects, table), :ets.match_spec_compile(spec))}
        end

      _ ->
        ETS.Set.select(table, spec)
    end
  end

  # A scan sees what the transaction wrote over what the table holds, in key order, as
  # Mnesia's add_match keeps an ordered set's.
  defp merge(objects, table) do
    case store_of(table) do
      {_group, ops} when ops != %{} ->
        objects
        |> Enum.reject(&Map.has_key?(ops, elem(&1, 0)))
        |> Enum.concat(for {_key, {:write, object}} <- ops, do: object)
        |> Enum.sort_by(&elem(&1, 0))

      _ ->
        objects
    end
  end

  # Inserted only when no object holds the key, the transaction's own writes counted.
  defp insert_new_object(table, {key, _} = object) do
    case store_of(table) do
      nil ->
        :ets.insert_new(table.table, object)

      {group, ops} ->
        taken =
          case Map.fetch(ops, key) do
            {:ok, {:write, _}} -> true
            {:ok, :delete} -> false
            :error -> :ets.member(table.table, key)
          end

        if taken do
          false
        else
          write_to_store(table, group, %{key => {:write, object}})
          true
        end
    end
  end

  defp put_objects(table, objects) do
    case store_of(table) do
      nil ->
        ETS.Set.put(table, objects)

      {group, _ops} ->
        write_to_store(
          table,
          group,
          Map.new(List.wrap(objects), fn {key, _} = object -> {key, {:write, object}} end)
        )

        {:ok, table}
    end
  end

  defp delete_object(table, key) do
    case store_of(table) do
      nil ->
        ETS.Set.delete(table, key)

      {group, _ops} ->
        write_to_store(table, group, %{key => :delete})
        {:ok, table}
    end
  end

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
  def set_as_of(_resource, query, as_of) do
    {:ok, %{query | as_of: as_of}}
  end

  @doc false
  @impl true
  def set_context(_resource, query, context) do
    {:ok, %{query | context: context}}
  end

  @doc false
  @impl true
  def select(query, select, _resource) do
    {:ok, %{query | select: select}}
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
    as_of = query.context[:private][:as_of]

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

                case load_aggregate_field(matches, field, resource, domain, context, as_of) do
                  {:ok, matches} ->
                    value =
                      aggregate_value(matches, kind, field, uniq?, include_nil?, default_value)

                    {:cont, {:ok, Map.put(acc, name, value)}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end

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

  # Records come straight from the table, so calculation and aggregate fields
  # have to be loaded before their values can be aggregated. They carry no `as_of`
  # of their own, so the load is given the one the query was read at.
  defp load_aggregate_field(records, field, resource, domain, context, as_of) do
    if is_atom(field) and not is_nil(Ash.Resource.Info.attribute(resource, field)) do
      {:ok, records}
    else
      Ash.load(records, field,
        domain: domain,
        tenant: context[:tenant],
        actor: context[:actor],
        as_of: as_of,
        authorize?: false
      )
    end
  end

  @doc false
  @impl true
  def combination_of(combinations, resource, domain) do
    {:ok, %Query{combination_of: combinations, resource: resource, domain: domain}}
  end

  @doc false
  @impl true
  def run_query(
        %Query{
          resource: resource,
          combination_of: combination_of,
          tenant: tenant
        } = query,
        _resource,
        parent \\ nil
      ) do
    # A fold can return the same primary key twice in two ways: `union_all`
    # keeps every row, and a `union` between parts carrying different
    # combination calculations keeps both copies, because the equality basis
    # is the combination fieldset. `runtime_sort/3` needs to know, or it
    # batches the sort-key load and cannot tell the copies apart.
    maybe_not_distinct? =
      Enum.any?(combination_of, fn {type, combination} ->
        type == :union_all or not Enum.empty?(combination.calculations)
      end)

    with {:ok, records} when records != [] <-
           get_records(resource, combination_of, parent, tenant),
         records <- Ash.Filter.Runtime.as_of_matches(records, resource, query.as_of),
         %Query{
           filter: filter,
           offset: offset,
           limit: limit,
           sort: sort,
           distinct: distinct,
           distinct_sort: distinct_sort,
           calculations: calculations,
           aggregates: aggregates,
           domain: domain,
           context: context
         } <- load_combinations(query),
         {:ok, records} <-
           filter_matches(
             records,
             filter,
             domain,
             context[:private][:tenant],
             context[:private][:actor],
             parent
           ),
         records <-
           distinct_and_sort(records, sort, distinct, distinct_sort,
             maybe_not_distinct?: maybe_not_distinct?,
             domain: domain,
             tenant: context[:private][:tenant]
           ),
         records <- Enum.drop(records, offset || []),
         records <- do_limit(records, limit),
         {:ok, records} <-
           do_add_aggregates(records, domain, resource, aggregates),
         {:ok, records} <-
           do_add_calculations(
             records,
             resource,
             calculations,
             domain
           ) do
      {:ok, records}
    else
      {:ok, records} ->
        {:ok, records}

      {:error, error} ->
        {:error, error}
    end
  end

  defp distinct_and_sort(records, sort, distinct, distinct_sort, opts) do
    opts = Keyword.put(opts, :rekey?, false)

    if distinct in [nil, []] do
      Sort.runtime_sort(records, sort, opts)
    else
      if distinct_sort in [nil, []] do
        records
        |> Sort.runtime_sort(sort, opts)
        |> Sort.runtime_distinct(distinct, opts)
      else
        records
        |> Sort.runtime_sort(distinct_sort, opts)
        |> Sort.runtime_distinct(distinct, opts)
        |> Sort.runtime_sort(sort, opts)
      end
    end
  end

  defp lateral_join_source(source_query, root_data, source_attribute, domain) do
    query =
      source_query.resource
      |> Ash.Query.new()
      |> Ash.Query.set_tenant(source_query.tenant)
      |> Ash.Query.as_of(source_query.as_of)
      |> Ash.Query.set_context(%{private: %{internal?: true}})
      |> Ash.Query.set_domain(domain)

    query =
      case Ash.Resource.Info.primary_key(query.resource) do
        [] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, source_attribute))

          Ash.Query.filter(query, ^ref(source_attribute) in ^source_attributes)

        [field] ->
          source_attributes = Enum.map(root_data, &Map.get(&1, field))
          Ash.Query.filter(query, ^ref(field) in ^source_attributes)

        fields ->
          filter = [
            or:
              Enum.map(root_data, fn record ->
                [and: Map.take(record, fields) |> Map.to_list()]
              end)
          ]

          Ash.Query.do_filter(query, filter)
      end

    with {:ok, data_layer_query} <- Ash.Query.data_layer_query(query) do
      run_query(data_layer_query, query.resource)
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
    primary_key = Ash.Resource.Info.primary_key(source_query.resource)

    source_query
    |> lateral_join_source(root_data, source_attribute, query.domain)
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
        {source_query, source_attribute, source_attribute_on_join_resource,
         %{type: :many_to_many} = relationship},
        {through_query, destination_attribute_on_join_resource, destination_attribute,
         _through_relationship}
      ]) do
    primary_key = Ash.Resource.Info.primary_key(source_query.resource)

    source_query
    |> lateral_join_source(root_data, source_attribute, query.domain)
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, root_data} ->
        destination_type =
          Ash.Resource.Info.attribute(query.resource, destination_attribute).type

        root_data
        |> Enum.reduce_while({:ok, []}, fn parent, {:ok, results} ->
          through_query
          |> resolve_parent(parent)
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
                        if Ash.Type.equal?(
                             destination_type,
                             Map.get(join_row, destination_attribute_on_join_resource),
                             Map.get(result, destination_attribute)
                           ) do
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

  # A through path: each step after the first is read once per record it is reached from, so
  # that `parent/1` in its query refers to that record.
  def run_query_with_lateral_join(query, root_data, _destination_resource, [
        {source_query, source_attribute, destination_attribute, relationship} | [_ | _] = rest
      ]) do
    primary_key = Ash.Resource.Info.primary_key(source_query.resource)

    source_query
    |> lateral_join_source(root_data, source_attribute, query.domain)
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, root_data} ->
        Enum.reduce_while(root_data, {:ok, []}, fn parent, {:ok, results} ->
          rest
          |> follow_through_path(
            [parent],
            source_attribute,
            destination_attribute,
            query.domain
          )
          |> case do
            {:ok, records, source_attribute, destination_attribute} ->
              values =
                records
                |> Enum.map(&Map.get(&1, source_attribute))
                |> Enum.reject(&is_nil/1)
                |> Enum.uniq()

              new_filter =
                if is_nil(query.filter) do
                  Ash.Filter.parse!(query.resource, [{destination_attribute, [in: values]}])
                else
                  Ash.Filter.add_to_filter!(query.filter, [{destination_attribute, [in: values]}])
                end

              case run_query(%{query | filter: new_filter}, relationship.source, parent) do
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

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
    end
  end

  defp follow_through_path([], records, source_attribute, destination_attribute, _domain) do
    {:ok, records, source_attribute, destination_attribute}
  end

  defp follow_through_path(
         [{through_query, next_source_attribute, next_destination_attribute, _} | rest],
         records,
         source_attribute,
         destination_attribute,
         domain
       ) do
    records
    |> Enum.reduce_while({:ok, []}, fn record, {:ok, acc} ->
      through_query
      |> resolve_parent(record)
      |> Ash.Query.filter(^ref(destination_attribute) == ^Map.get(record, source_attribute))
      |> Ash.Query.set_context(%{private: %{internal?: true}})
      |> Ash.Query.set_domain(domain)
      |> Ash.read(authorize?: false)
      |> case do
        {:ok, through_data} -> {:cont, {:ok, through_data ++ acc}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, through_data} ->
        primary_key = Ash.Resource.Info.primary_key(through_query.resource)

        follow_through_path(
          rest,
          Enum.uniq_by(through_data, &Map.take(&1, primary_key)),
          next_source_attribute,
          next_destination_attribute,
          domain
        )

      {:error, error} ->
        {:error, error}
    end
  end

  # Replaces `parent/1` expressions in the query's filter with their values for `parent`.
  defp resolve_parent(%{filter: nil} = query, _parent), do: query

  defp resolve_parent(query, parent) do
    filter =
      Ash.Filter.map(query.filter, fn
        %Ash.Query.Parent{expr: expr} ->
          case Ash.Expr.eval(expr,
                 record: parent,
                 resource: parent.__struct__,
                 unknown_on_unknown_refs?: true
               ) do
            {:ok, value} ->
              value

            {:error, error} ->
              throw({:resolve_parent_error, error})

            :unknown ->
              throw(
                {:resolve_parent_error,
                 "could not resolve a `parent/1` expression in a through relationship step"}
              )
          end

        other ->
          other
      end)

    %{query | filter: filter}
  catch
    {:resolve_parent_error, error} ->
      Ash.Query.add_error(query, error)
  end

  def do_add_calculations(records, _resource, [], _domain), do: {:ok, records}

  def do_add_calculations([], _resource, _calculations, _domain), do: {:ok, []}

  # Everything the calculations refer to is loaded up front, for all records at once, and the
  # calculations are then evaluated against the loaded records without loading anything lazily.
  # Calculations that the expressions refer to are loaded under names scoped to the evaluation
  # (see `Ash.Filter.Runtime.runtime_calculation_name/1`), so that each reference gets the value
  # for its own arguments. The loaded records are only used to evaluate the expressions: the
  # values are put on the records as they were given.
  def do_add_calculations(records, resource, calculations, domain) do
    with {:ok, calculations} <- hydrate_calculation_expressions(calculations, resource) do
      calculations
      |> Enum.group_by(fn {calculation, _expression} ->
        {calculation.context.actor, calculation.context.tenant}
      end)
      |> Enum.reduce_while({:ok, records}, fn {{actor, tenant}, calculations}, {:ok, records} ->
        expressions = Enum.map(calculations, &elem(&1, 1))

        with {:ok, loaded_records} <-
               Ash.Filter.Runtime.load_for_expressions(
                 records,
                 expressions,
                 resource,
                 domain,
                 actor,
                 tenant
               ),
             {:ok, records} <-
               add_calculation_values(records, loaded_records, resource, calculations) do
          {:cont, {:ok, records}}
        else
          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
    end
  end

  defp hydrate_calculation_expressions(calculations, resource) do
    calculations
    |> Enum.reduce_while({:ok, []}, fn {calculation, expression}, {:ok, calculations} ->
      case Ash.Filter.hydrate_refs(expression, %{resource: resource, public?: false}) do
        {:ok, expression} ->
          {:cont, {:ok, [{calculation, expression} | calculations]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, calculations} -> {:ok, Enum.reverse(calculations)}
      {:error, error} -> {:error, error}
    end
  end

  defp add_calculation_values(records, loaded_records, resource, calculations) do
    records
    |> Enum.zip(loaded_records)
    |> Enum.reduce_while({:ok, []}, fn {record, loaded_record}, {:ok, records} ->
      calculations
      |> Enum.reduce_while({:ok, record}, fn {calculation, expression}, {:ok, record} ->
        case Ash.Expr.eval_hydrated(expression,
               record: loaded_record,
               resource: resource,
               actor: calculation.context.actor,
               tenant: calculation.context.tenant
             ) do
          {:ok, value} ->
            {:cont, {:ok, put_calculation_value(record, calculation, value)}}

          :unknown ->
            {:cont, {:ok, put_calculation_value(record, calculation, nil)}}

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

  defp put_calculation_value(record, %{load: load}, value) when not is_nil(load) do
    Map.put(record, load, value)
  end

  defp put_calculation_value(record, calculation, value) do
    Map.update!(record, :calculations, &Map.put(&1, calculation.name, value))
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
            join_filters: join_filters,
            related?: related?
          } = _aggregate,
          {:ok, record} ->
            if related? do
              with {:ok, loaded_record} <-
                     Ash.load(
                       record,
                       record.__struct__
                       |> Ash.Query.load(
                         relationship_path_to_load(
                           relationship_path,
                           Ash.Query.set_context(Ash.Query.unset(query, :load), %{
                             private: %{authorize?: false}
                           })
                         )
                       )
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
                   sorted <-
                     Sort.runtime_sort(filtered, query.sort,
                       domain: domain,
                       tenant: context[:tenant],
                       rekey?: false
                     ) do
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
            else
              updated_filter =
                if query.filter do
                  result =
                    Ash.Filter.map(query.filter, fn
                      %Ash.Query.Parent{expr: expr} ->
                        case Ash.Expr.eval(expr, record: record) do
                          {:ok, value} -> value
                          value -> value
                        end

                      other ->
                        other
                    end)

                  result
                else
                  nil
                end

              updated_query = %{query | filter: updated_filter, sort: query.sort || []}

              case Ash.aggregate(
                     updated_query,
                     {name, kind, if(field, do: [field: field], else: [])},
                     domain: domain,
                     tenant: context[:tenant],
                     actor: context[:actor],
                     authorize?: context[:authorize?] || false
                   ) do
                {:ok, results} ->
                  value =
                    case Map.get(results, name) do
                      nil -> default_value
                      value -> value
                    end

                  if load do
                    {:cont, {:ok, Map.put(record, load, value)}}
                  else
                    {:cont, {:ok, Map.update!(record, :aggregates, &Map.put(&1, name, value))}}
                  end

                {:error, error} ->
                  {:halt, {:error, error}}
              end
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
              nil

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
        values = Stream.map(records, &field_value(&1, field))

        items =
          if uniq? do
            values |> Stream.uniq() |> Stream.reject(&is_nil/1)
          else
            values |> Stream.reject(&is_nil/1)
          end

        # Whether there is anything to aggregate has to be decided after the
        # `nil`s are rejected, not before. Records that all hold `nil` leave
        # nothing to fold, exactly as no records at all do, and both should
        # give the aggregate's default rather than one giving `Enum.sum([])`
        # and the other raising out of `Enum.max/2`.
        case Enum.take(items, 1) do
          [] ->
            nil

          [first_item] ->
            case kind do
              :sum ->
                if is_struct(first_item, Decimal) do
                  Enum.reduce(items, Decimal.new(0), &Decimal.add(&1, &2))
                else
                  Enum.sum(items)
                end

              # `Comp` is what `Ash.Actions.Sort.runtime_sort/3` orders with, so
              # comparing with it here keeps `min`/`max` consistent with how this
              # data layer sorts the very same field. `Enum.max/1` would fall
              # back to Erlang term order, which compares a struct's fields in
              # alphabetical key order — for a `DateTime` that means `:day`
              # decides and `:year` is never reached. No field ordering would be
              # right anyway: `DateTime.compare/2` applies the UTC offsets, so
              # two rows with different wall clocks can be the same instant.
              :max ->
                Enum.max(items, Comp)

              :min ->
                Enum.min(items, Comp)
            end
        end
    end
    |> case do
      nil -> default
      other -> other
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

  defp get_records(resource, [], _parent, tenant) do
    with :ok <- check_deadline(resource),
         {:ok, table} <- wrap_or_create_table(resource, tenant, false),
         {:ok, record_tuples} <- all_objects(table) do
      cast_records(Enum.map(record_tuples, &elem(&1, 1)), resource)
    else
      :no_table -> {:ok, []}
      {:error, error} -> {:error, error}
    end
  end

  defp get_records(resource, [{_, first} | _] = combinations_of, parent, tenant) do
    field_set =
      Enum.uniq_by(
        Enum.map(
          first.select ||
            MapSet.to_list(Ash.Resource.Info.selected_by_default_attribute_names(resource)),
          &Ash.Resource.Info.attribute(resource, &1)
        ) ++
          Enum.map(first.calculations, &elem(&1, 0)),
        & &1.name
      )

    # `field_set` holds field structs while these are names, so the two have to
    # be compared by name. A combination calculation whose name shadows a field
    # of the resource is promoted onto the record itself below; anything else
    # stays in `:calculations`.
    field_set_names = Enum.map(field_set, & &1.name)

    fields_to_rewrite =
      resource
      |> Ash.Resource.Info.fields([:attributes, :calculations, :aggregates])
      |> Enum.map(& &1.name)
      |> Enum.filter(&(&1 in field_set_names))

    rewrite? = not Enum.empty?(fields_to_rewrite)

    {simple_equality, non_simple_equality} =
      Enum.split_with(field_set, fn %{type: type} ->
        is_nil(type) || Ash.Type.simple_equality?(type)
      end)

    {base, matcher, grouper} =
      case {simple_equality, non_simple_equality} do
        {[], non_simple_equality} ->
          non_simple_equality = Enum.map(non_simple_equality, & &1.name)
          base = []

          matcher = fn record, acc ->
            Enum.any?(acc, fn existing ->
              Enum.all?(non_simple_equality, fn %{type: type, name: name} ->
                Ash.Type.equal?(type, combo_field(record, name), combo_field(existing, name))
              end)
            end)
          end

          grouper = fn records, acc ->
            Enum.concat(acc, records)
          end

          {base, matcher, grouper}

        {simple_equality, []} ->
          simple_equality_names = Enum.map(simple_equality, & &1.name)
          base = MapSet.new()
          matcher = fn record, acc -> combo_fields(record, simple_equality_names) in acc end

          grouper = fn records, acc ->
            Enum.reduce(records, acc, fn record, acc ->
              MapSet.put(acc, combo_fields(record, simple_equality_names))
            end)
          end

          {base, matcher, grouper}

        {simple_equality, non_simple_equality} ->
          simple_equality_names = Enum.map(simple_equality, & &1.name)
          non_simple_equality_names = Enum.map(non_simple_equality, & &1.name)
          base = Map.new()

          matcher = fn record, acc ->
            key = combo_fields(record, simple_equality_names)

            case Map.fetch(acc, key) do
              {:ok, non_simple_equality_values} ->
                Enum.all?(non_simple_equality, fn %{type: type, name: name} ->
                  Ash.Type.equal?(
                    type,
                    combo_field(record, name),
                    Map.get(non_simple_equality_values, name)
                  )
                end)

              :error ->
                false
            end
          end

          grouper = fn records, acc ->
            Enum.reduce(records, acc, fn record, acc ->
              Map.put(
                acc,
                combo_fields(record, simple_equality_names),
                combo_fields(record, non_simple_equality_names)
              )
            end)
          end

          {base, matcher, grouper}
      end

    combinations_of
    |> Enum.reduce_while({:ok, [], base}, fn {type, combination}, {:ok, records, acc} ->
      case run_query(
             %{combination | tenant: tenant},
             resource,
             parent
           ) do
        {:ok, results} ->
          results =
            if rewrite? do
              Enum.map(results, fn record ->
                {merge, remain} = Map.split(record.calculations, fields_to_rewrite)

                record
                |> Map.merge(merge)
                |> Map.put(:calculations, remain)
              end)
            else
              results
            end

          case type do
            type when type in [:base, :union_all] ->
              {:cont, {:ok, [records, results], grouper.(results, acc)}}

            :union ->
              new_results =
                Enum.reject(results, fn result ->
                  matcher.(result, acc)
                end)

              {:cont, {:ok, [records, new_results], grouper.(new_results, acc)}}

            :intersect ->
              temp_results_acc =
                results
                |> Enum.filter(fn result ->
                  matcher.(result, acc)
                end)
                |> grouper.(base)

              records =
                records
                |> Stream.flat_map(&List.flatten(List.wrap(&1)))
                |> Enum.filter(fn result ->
                  matcher.(result, temp_results_acc)
                end)

              {:cont, {:ok, records, grouper.(records, base)}}

            :except ->
              temp_results_acc = grouper.(results, base)

              results =
                records
                |> Stream.flat_map(&List.flatten(List.wrap(&1)))
                |> Enum.reject(fn result ->
                  matcher.(result, temp_results_acc)
                end)

              {:cont, {:ok, results, grouper.(results, base)}}
          end

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, records, _acc} ->
        records
        |> List.flatten()
        |> then(&{:ok, &1})

      {:error, error} ->
        {:error, error}
    end
  end

  defp load_combinations(query) do
    %{
      query
      | filter: do_load_combinations(query.filter),
        sort:
          Enum.map(query.sort, fn
            {%Ash.Query.Calculation{
               module: Ash.Resource.Calculation.Expression,
               opts: opts
             } = calc, order} ->
              {%{calc | opts: Keyword.update!(opts, :expr, &do_load_combinations/1)}, order}

            other ->
              other
          end),
        distinct:
          Enum.map(query.distinct, fn
            {%Ash.Query.Calculation{
               module: Ash.Resource.Calculation.Expression,
               opts: opts
             } = calc, order} ->
              {%{calc | opts: Keyword.update!(opts, :expr, &do_load_combinations/1)}, order}

            other ->
              other
          end),
        distinct_sort:
          query.distinct_sort &&
            Enum.map(query.distinct_sort, fn
              {%Ash.Query.Calculation{
                 module: Ash.Resource.Calculation.Expression,
                 opts: opts
               } = calc, order} ->
                {%{calc | opts: Keyword.update!(opts, :expr, &do_load_combinations/1)}, order}

              other ->
                other
            end)
    }
  end

  defp do_load_combinations(filter) do
    Ash.Filter.map(filter, fn
      %Ash.Query.Ref{
        combinations?: true,
        attribute: %{load?: false} = attr
      } = ref ->
        %{ref | attribute: %{attr | load?: true}}

      other ->
        other
    end)
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

  defp combo_field(record, field) do
    Map.get(record.calculations, field, Map.get(record, field))
  end

  defp combo_fields(record, fields) do
    Map.new(fields, fn field ->
      {field, combo_field(record, field)}
    end)
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

            _ ->
              {:halt,
               {:error,
                Ash.Error.Invalid.InvalidStoredData.exception(
                  resource: resource,
                  field: attribute.name
                )}}
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
  def upsert(resource, changeset, keys, identity \\ nil) do
    transact_write(resource, changeset.tenant, true, fn ->
      do_upsert(resource, changeset, keys, identity)
    end)
  end

  # An upsert locks the value it looks its record up by, its identity's or its primary key's,
  # before the lookup that decides whether it creates or updates.
  defp do_upsert(resource, changeset, keys, identity, from_bulk_create? \\ false) do
    pkey = Ash.Resource.Info.primary_key(resource)
    keys = keys || pkey

    if (is_nil(identity) || !identity.nils_distinct?) &&
         Enum.any?(keys, &is_nil(Ash.Changeset.get_attribute(changeset, &1))) do
      create(resource, changeset, from_bulk_create?)
    else
      values =
        Enum.map(keys, fn key ->
          {key,
           Ash.Changeset.get_attribute(changeset, key) || Map.get(changeset.params, key) ||
             Map.get(changeset.params, to_string(key))}
        end)

      lock_item(
        resource,
        changeset.tenant,
        if(identity,
          do: identity_lock(resource, identity, values),
          else: {:record, Map.new(values)}
        )
      )

      key_filters =
        Enum.map(values, fn
          {key, nil} -> {key, [is_nil: true]}
          pair -> pair
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

      to_set =
        changeset
        |> Ash.Changeset.set_on_upsert(keys)
        |> apply_upsert_update_defaults(resource, changeset)

      resource
      |> resource_to_query(changeset.domain)
      |> Map.put(:filter, query.filter)
      |> Map.put(:tenant, changeset.tenant)
      |> Map.put(:as_of, upsert_instant(resource, changeset))
      |> run_query(resource)
      |> case do
        {:ok, []} ->
          resource
          |> create(changeset, from_bulk_create?)
          |> set_upsert_action(:create)

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

            resource
            |> update(
              %{changeset | action_type: :update, filter: nil},
              pkey_map(resource, result),
              from_bulk_create?
            )
            |> set_upsert_action(:update)
          else
            {:ok, []} ->
              skipped_upsert(resource, changeset, result)

            {:error, reason} ->
              {:error, reason}
          end

        {:ok, _} ->
          {:error, "Multiple records matching keys"}
      end
    end
  end

  @doc false
  # The record an upsert skipped is only returned from the changeset's tenant: an upsert on the
  # primary key or an `all_tenants?` identity can conflict with a record in another tenant
  def skipped_upsert(resource, changeset, result) do
    tenant_filter =
      case Ash.Actions.Helpers.upsert_tenant_filter(changeset) do
        nil -> nil
        tenant_filter -> Ash.Filter.parse!(resource, tenant_filter)
      end

    case filter_matches([result], tenant_filter, changeset.domain, changeset.tenant, nil) do
      {:ok, [result]} ->
        {:ok, Ash.Resource.put_metadata(result, :upsert_skipped, true)}

      {:ok, []} ->
        {:error,
         Ash.Error.Changes.StaleRecord.exception(resource: resource, filter: changeset.filter)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp set_upsert_action({:ok, record}, action) do
    {:ok, Ash.Resource.put_metadata(record, :upsert_action, action)}
  end

  defp set_upsert_action(result, _), do: result

  defp apply_upsert_update_defaults(to_set, resource, changeset) do
    touch_update_defaults? =
      changeset.context[:private][:touch_update_defaults?]

    update_default_attrs =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.filter(& &1.update_default)

    if touch_update_defaults? == false || to_set == [] do
      upsert_fields = changeset.context[:private][:upsert_fields]
      update_default_names = MapSet.new(update_default_attrs, & &1.name)

      Keyword.reject(to_set, fn {key, _} ->
        MapSet.member?(update_default_names, key) &&
          !explicitly_set?(key, upsert_fields, changeset)
      end)
    else
      # Add update_defaults that aren't already in to_set
      # (set_on_upsert's upsert_fields branch doesn't include them)
      Enum.reduce(update_default_attrs, to_set, fn attr, acc ->
        if Keyword.has_key?(acc, attr.name) do
          acc
        else
          value =
            case attr.update_default do
              function when is_function(function) -> function.()
              {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) -> apply(m, f, a)
              value -> value
            end

          Keyword.put(acc, attr.name, value)
        end
      end)
    end
  end

  defp explicitly_set?(key, upsert_fields, _changeset) when is_list(upsert_fields),
    do: key in upsert_fields

  defp explicitly_set?(key, _, changeset),
    do: Map.has_key?(changeset.attributes, key) && key not in Map.get(changeset, :defaults, [])

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

  @impl true
  def bulk_create(resource, stream, options) do
    stream = Enum.to_list(stream)
    log_bulk_create(resource, stream, options)

    transact_write(resource, options.tenant, true, fn ->
      if options[:upsert?] do
        # This is not optimized, but that's okay for now
        stream
        |> Enum.reduce_while({:ok, []}, fn changeset, {:ok, results} ->
          changeset =
            Ash.Changeset.set_context(changeset, %{
              private: %{
                upsert_fields: options[:upsert_fields] || [],
                touch_update_defaults?: Map.get(options, :touch_update_defaults?, true)
              }
            })

          case do_upsert(
                 resource,
                 changeset,
                 options.upsert_keys,
                 options.identity,
                 true
               ) do
            {:ok, result} ->
              if Ash.Resource.get_metadata(result, :upsert_skipped) do
                {:cont, {:ok, results}}
              else
                result = Ash.Actions.Helpers.Bulk.put_metadata(result, changeset)

                {:cont, {:ok, [result | results]}}
              end

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)
        |> case do
          {:ok, result} -> {:ok, Enum.reverse(result)}
          other -> other
        end
      else
        # Every record of the batch is locked before the stored periods are read, so the read
        # stays true for the whole batch.
        with {:ok, prepared} <-
               Enum.reduce_while(stream, {:ok, []}, fn changeset, {:ok, prepared} ->
                 with {:ok, valid_pkey} <- get_valid_pkey(resource, changeset),
                      {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
                      {:ok, record} <- apply_atomics(changeset, resource, record),
                      {:ok, record} <- establish_period(record, resource, changeset),
                      record <- unload_relationships(resource, record),
                      :ok <- check_non_empty(resource, record),
                      :ok <- lock_record(resource, options.tenant, record) do
                   {:cont, {:ok, [{changeset, valid_pkey, record} | prepared]}}
                 else
                   {:error, error} -> {:halt, {:error, error}}
                 end
               end),
             {:ok, table} <- written_table(resource, options.tenant, true),
             {:ok, stored} <- stored_index(table, resource) do
          prepared
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, [], stored}, fn {changeset, valid_pkey, record},
                                                     {:ok, results, known} ->
            with :ok <-
                   enforce_identities(
                     resource,
                     nil,
                     record,
                     [earlier: Enum.map(results, &elem(&1, 3))] ++ identity_opts(changeset)
                   ),
                 :ok <- check_non_overlapping(resource, record, known) do
              {:cont,
               {:ok,
                [
                  {create_key(resource, record, valid_pkey), changeset.context.bulk_create.index,
                   changeset.context.bulk_create.ref, record}
                  | results
                ], remember_period(known, resource, record)}}
            else
              {:error, error} ->
                {:halt, {:error, error}}
            end
          end)
          |> case do
            {:ok, records, _known} ->
              case put_or_insert_new_batch(table, records, resource, options.return_records?) do
                :ok ->
                  :ok

                {:ok, records} ->
                  records
                  |> Enum.reduce([], fn record, records ->
                    [set_loaded(record) | records]
                  end)
                  |> then(&{:ok, &1})

                {:error, error} ->
                  {:error, error}
              end

            {:error, error} ->
              {:error, error}
          end
        end
      end
    end)
  end

  @doc false
  @impl true
  def create(resource, changeset, from_bulk_create? \\ false) do
    transact_write(resource, changeset.tenant, true, fn ->
      with {:ok, valid_pkey} <- get_valid_pkey(resource, changeset),
           _ <- if(!from_bulk_create?, do: log_create(resource, changeset)),
           {:ok, record} <- Ash.Changeset.apply_attributes(changeset),
           {:ok, record} <- apply_atomics(changeset, resource, record),
           {:ok, record} <- establish_period(record, resource, changeset),
           record <- unload_relationships(resource, record),
           :ok <- check_non_empty(resource, record),
           :ok <- lock_record(resource, changeset.tenant, record),
           {:ok, table} <- written_table(resource, changeset.tenant, true),
           :ok <- enforce_identities(resource, nil, record, identity_opts(changeset)),
           {:ok, stored} <- stored_periods(table, resource, record),
           :ok <- check_non_overlapping(resource, record, stored),
           {:ok, record} <-
             put_or_insert_new(
               table,
               {create_key(resource, record, valid_pkey), record},
               resource
             ) do
        {:ok, set_loaded(record)}
      else
        {:error, error} -> {:error, error}
      end
    end)
  end

  defp apply_atomics(%{create_atomics: create_atomics} = changeset, resource, record)
       when create_atomics != [] do
    case make_atomics(create_atomics, resource, changeset.domain, record) do
      {:ok, atomic_values} ->
        {:ok, struct(record, atomic_values)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp apply_atomics(_changeset, _resource, record), do: {:ok, record}

  # The instant is this layer's: a core `now()` resolved earlier could precede the record.
  defp establish_period(record, resource, changeset) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        {:ok, record}

      attribute ->
        {:ok,
         put_established_period(
           record,
           attribute,
           Map.get(record, attribute),
           resource,
           changeset
         )}
    end
  end

  defp check_non_empty(resource, record) do
    with period when not is_nil(period) <- Ash.Resource.Info.temporal_attribute(resource),
         %Ash.Range{} = value <- Map.get(record, period),
         true <- Ash.Range.empty?(value) do
      {:error,
       Ash.Error.Changes.InvalidAttribute.exception(
         field: period,
         value: value,
         message: "is empty, so the record could not be read at any point in time"
       )}
    else
      _ -> :ok
    end
  end

  # A resource in a transaction group has its identities checked at the write, against the
  # stored records as Ash's identity check reads them, so the check and the write it guards are
  # one step, which a check at read cannot be while reads take no locks. A write locks each
  # identity value it takes, so writes taking one value run one after another, and is refused
  # when a stored record holds the value. A value the record held before the write is not
  # checked again. A bulk create also checks the records before it in its batch, and an upsert
  # leaves its own identity to the lookup that decides it.
  defp enforce_identities(resource, before, written, opts) do
    if Ash.DataLayer.Ets.Info.transaction_group(resource) do
      resource
      |> Ash.Resource.Info.identities()
      |> Enum.reject(&(&1.name == opts[:upsert_identity]))
      |> Enum.reduce_while(:ok, fn identity, :ok ->
        values = identity_values(resource, identity, written, opts)

        with false <- is_nil(values),
             false <-
               not is_nil(before) and identity_values(resource, identity, before, opts) == values,
             :ok <- lock_item(resource, opts[:tenant], identity_lock(resource, identity, values)),
             {:ok, false} <- identity_taken?(resource, identity, values, written, opts) do
          {:cont, :ok}
        else
          {:ok, true} ->
            {:halt,
             {:error,
              Ash.Error.Changes.InvalidChanges.exception(
                fields: identity.field_names || identity.keys,
                message: identity.message || "has already been taken"
              )}}

          {:error, error} ->
            {:halt, {:error, error}}

          true ->
            {:cont, :ok}
        end
      end)
    else
      :ok
    end
  end

  # What a record holds an identity by, as Ash defines an identity: its keys, with the tenant
  # attribute among them under attribute multitenancy unless the identity spans tenants (as
  # Ash.Actions.Create puts it among an upsert's keys), on a record the resource's base filter
  # and the identity's `where` admit, with no nil key where the identity keeps nils distinct.
  # `nil` when the identity does not count the record.
  defp identity_values(resource, identity, record, opts) do
    if Enum.any?(identity.keys, &Ash.Resource.Info.calculation(resource, &1)) do
      raise ArgumentError, "Cannot check an identity based on calculated fields."
    end

    keys =
      if !identity.all_tenants? and
           Ash.Resource.Info.multitenancy_strategy(resource) == :attribute,
         do: Enum.uniq([Ash.Resource.Info.multitenancy_attribute(resource) | identity.keys]),
         else: identity.keys

    values = Enum.map(keys, &{&1, Map.get(record, &1)})
    filter = resource |> Ash.Query.do_filter(identity.where) |> Map.get(:filter)

    with false <- identity.nils_distinct? and Enum.any?(values, &is_nil(elem(&1, 1))),
         {:ok, [_]} <- filter_matches([record], filter, opts[:domain], opts[:tenant], nil) do
      values
    else
      _ -> nil
    end
  end

  # An identity value's lock names it in its types' comparable form, the form Ash compares and
  # keys values by (`Ash.Type.to_simple_equality_comparable/2`). A key whose type has none
  # locks the identity whole.
  defp identity_lock(resource, identity, values) do
    comparable =
      Enum.map(values, fn {key, value} ->
        type = Ash.Resource.Info.attribute(resource, key).type

        if Ash.Type.simple_equality_comparable?(type),
          do: {:ok, Ash.Type.to_simple_equality_comparable(type, value)}
      end)

    if Enum.all?(comparable),
      do: {:identity, identity.name, Enum.map(comparable, &elem(&1, 1))},
      else: {:identity, identity.name}
  end

  # Whether a record other than the one written holds the value, its other versions included.
  defp identity_taken?(resource, identity, values, written, opts) do
    filter =
      resource
      |> Ash.Query.do_filter(
        Enum.map(values, fn
          {key, nil} -> {key, [is_nil: true]}
          pair -> pair
        end)
      )
      |> Ash.Query.do_filter(identity.where)
      |> then(fn query ->
        case Ash.Resource.Info.primary_key(resource) do
          [] -> query
          _ -> Ash.Query.do_filter(query, not: Map.to_list(primary_key(resource, written)))
        end
      end)
      |> Map.get(:filter)

    tenant = if !identity.all_tenants?, do: opts[:tenant]

    with {:ok, stored} <-
           resource
           |> resource_to_query(opts[:domain])
           |> Map.merge(%{filter: filter, tenant: tenant, as_of: opts[:as_of], limit: 1})
           |> run_query(resource),
         {:ok, batch} <- filter_matches(opts[:earlier] || [], filter, opts[:domain], tenant, nil) do
      {:ok, stored != [] or batch != []}
    end
  end

  defp identity_opts(changeset) do
    [
      tenant: changeset.tenant,
      domain: changeset.domain,
      as_of: changeset.as_of,
      upsert_identity: changeset.context[:private][:upsert_identity]
    ]
  end

  # Outside a transaction group the look and the write are separate, so concurrent creates can
  # both land; a group's create holds its record's lock across both.
  defp check_non_overlapping(resource, record, known) do
    with period when not is_nil(period) <- Ash.Resource.Info.temporal_attribute(resource),
         %Ash.Range{} = period_value <- Map.get(record, period),
         periods = Map.get(known, primary_key(resource, record), []),
         true <- Enum.any?(periods, &overlapping?(&1, period_value)) do
      {:error,
       Ash.Error.Changes.InvalidAttribute.exception(
         field: period,
         value: period_value,
         message: "overlaps the period of an existing version of this record"
       )}
    else
      _ -> :ok
    end
  end

  defp overlapping?(stored, period_value) do
    match?(%Ash.Range{}, stored) and Ash.Range.intersects?(stored, period_value)
  end

  # A map pattern matches keys containing it, so only this record's versions are read.
  defp stored_periods(table, resource, record) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        {:ok, %{}}

      period ->
        primary_key = primary_key(resource, record)

        with {:ok, periods} <-
               select_objects(table, [{{Map.put(primary_key, period, :"$1"), :_}, [], [:"$1"]}]) do
          {:ok, %{primary_key => periods}}
        end
    end
  end

  # Nothing is written until the reduce ends, so one read covers the batch.
  defp stored_index(table, resource) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        {:ok, %{}}

      period ->
        with {:ok, keys} <- select_objects(table, [{{:"$1", :_}, [], [:"$1"]}]) do
          {:ok, Enum.group_by(keys, &Map.drop(&1, [period]), &Map.get(&1, period))}
        end
    end
  end

  # Versions earlier in the batch are not in the table yet.
  defp remember_period(known, resource, record) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        known

      period ->
        value = Map.get(record, period)
        Map.update(known, primary_key(resource, record), [value], &[value | &1])
    end
  end

  defp primary_key(resource, record) do
    Map.take(record, Ash.Resource.Info.primary_key(resource))
  end

  defp put_established_period(record, _attribute, %Ash.Range{}, _resource, _changeset), do: record

  defp put_established_period(record, attribute, nil, resource, changeset) do
    case Ash.Temporal.write_period(resource, changeset.as_of) do
      {:ok, period} -> Map.put(record, attribute, period)
      :error -> record
    end
  end

  defp put_established_period(record, _attribute, _other, _resource, _changeset), do: record

  # Without an instant the query sees every version, not the one holding it.
  defp upsert_instant(resource, changeset) do
    case Ash.Temporal.write_instant(resource, changeset.as_of) do
      {:ok, instant} -> instant
      :error -> nil
    end
  end

  defp set_loaded(%resource{} = record) do
    %{record | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: resource}}
  end

  defp put_or_insert_new(table, {pkey, record}, resource) do
    attributes = resource |> Ash.Resource.Info.attributes()

    case dump_to_native(record, attributes) do
      {:ok, casted} ->
        if insert_new_object(table, {pkey, casted}) do
          {:ok, {_key, record}} = get_object(table, pkey)
          cast_record(record, resource)
        else
          {:error, pkey_already_taken_error(resource)}
        end

      other ->
        other
    end
  end

  defp pkey_already_taken_error(resource) do
    Ash.Error.Changes.InvalidChanges.exception(
      fields: Ash.Resource.Info.primary_key(resource),
      message: "has already been taken"
    )
  end

  defp put_or_insert_new_batch(table, records, resource, return_records?) do
    attributes = resource |> Ash.Resource.Info.attributes()

    Enum.reduce_while(records, {:ok, [], []}, fn {pkey, index, ref, record},
                                                 {:ok, acc, indices} ->
      case dump_to_native(record, attributes) do
        {:ok, casted} ->
          {:cont, {:ok, [{pkey, casted} | acc], [{pkey, index, ref} | indices]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, batch, indices} ->
        case put_objects(table, batch) do
          {:ok, set} ->
            if return_records? do
              Enum.reduce_while(indices, {:ok, []}, fn {pkey, index, ref}, {:ok, acc} ->
                {:ok, {_key, record}} = get_object(set, pkey)

                case cast_record(record, resource) do
                  {:ok, casted} ->
                    casted =
                      Ash.Actions.Helpers.Bulk.put_metadata(
                        casted,
                        index,
                        ref,
                        :bulk_create_index
                      )

                    {:cont, {:ok, [casted | acc]}}

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

  # Upstream's fix (v3.32.2): a resource with NO primary key gave every record
  # the same empty key, so creates overwrote one another. The synthetic ref is
  # per-record, so keyless records stop colliding.
  #
  # ⚠️ This validates and supplies a key; it does NOT decide the STORED key for
  # a keyed resource - `create_key/3` does, because ours carries the period.
  defp get_valid_pkey(resource, changeset) do
    case Ash.Resource.Info.primary_key(resource) do
      [] ->
        {:ok, %{__ash_synthetic_key__: make_ref()}}

      pkey_fields ->
        pkey =
          Enum.into(pkey_fields, %{}, fn attr ->
            {attr, Ash.Changeset.get_attribute(changeset, attr)}
          end)

        if Enum.any?(pkey, fn {_, v} -> is_nil(v) end) do
          {:error, InvalidPrimaryKey.exception(resource: resource, value: pkey)}
        else
          {:ok, pkey}
        end
    end
  end

  # The stored key. A keyed resource keys on the RECORD, not the changeset:
  # `key_fields/1` appends the temporal attribute, and the period is only
  # established once `establish_period/3` has run. A keyless resource has
  # nothing to key on, so it takes the synthetic ref above.
  defp create_key(resource, record, synthetic) do
    case Ash.Resource.Info.primary_key(resource) do
      [] -> synthetic
      _ -> pkey_map(resource, record)
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
        |> once_per_record(resource, changeset)
        |> Enum.reduce_while(acc, fn result, acc ->
          transact_write(resource, query.tenant, false, fn ->
            case still_matching(resource, changeset, query, result) do
              {:ok, nil} ->
                :skipped

              {:ok, current} ->
                result_changeset = %{changeset | data: current}

                with :ok <- destroy(query.resource, result_changeset),
                     do: {:ok, Ash.Actions.Helpers.Bulk.put_metadata(current, result_changeset)}

              {:error, error} ->
                {:error, error}
            end
          end)
          |> case do
            :skipped ->
              {:cont, acc}

            {:ok, result} ->
              case acc do
                :ok -> {:cont, :ok}
                {:ok, results} -> {:cont, {:ok, [result | results]}}
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

  # A record a bulk write matched before its lock was held is read again once it is, and left
  # out when it is gone or no longer matches the query, as an update reads the stored record
  # again and checks the changeset's filter against it at the write.
  defp still_matching(resource, changeset, query, record) do
    with %{} <-
           Process.get({:ash_ets_transaction, Ash.DataLayer.Ets.Info.transaction_group(resource)}),
         :ok <- lock_record(resource, query.tenant, record),
         {:ok, table} <- written_table(resource, query.tenant, false),
         pkey =
           holding_key(
             table,
             resource,
             record,
             supersession(resource, changeset),
             match?(%Ash.Range{}, changeset.as_of)
           ),
         {:ok, {_key, stored}} when is_map(stored) <- get_object(table, pkey),
         {:ok, stored} <- cast_record(stored, resource),
         {:ok, matched} <-
           filter_matches(
             [stored],
             query.filter,
             changeset.domain,
             query.tenant,
             changeset.context[:private][:actor]
           ) do
      {:ok, List.first(matched)}
    else
      nil -> {:ok, record}
      {:error, error} -> {:error, error}
      _gone -> {:ok, nil}
    end
  end

  defp once_per_record(results, resource, changeset) do
    case changeset.as_of do
      %Ash.Range{} = as_of ->
        results
        |> Enum.filter(&touches_portion?(resource, &1, as_of))
        |> then(&first_versions(resource, &1))

      _ ->
        results
    end
  end

  @doc false
  @impl true
  def destroy(resource, %{data: record, filter: filter} = changeset) do
    transact_write(resource, changeset.tenant, false, fn ->
      do_destroy(
        resource,
        record,
        changeset.tenant,
        filter,
        changeset.domain,
        changeset.context[:private][:actor],
        supersession(resource, changeset),
        match?(%Ash.Range{}, changeset.as_of)
      )
    end)
  end

  defp do_destroy(resource, record, tenant, filter, domain, actor, supersede, range?) do
    :ok = lock_record(resource, tenant, record)

    case written_table(resource, tenant, false) do
      {:ok, table} ->
        do_destroy(table, resource, record, tenant, filter, domain, actor, supersede, range?)

      # Nothing has ever been written for this tenant, so there is nothing to destroy.
      :no_table ->
        if has_filter?(filter) do
          {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, filter: filter)}
        else
          :ok
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_destroy(table, resource, record, tenant, filter, domain, actor, supersede, range?) do
    pkey = holding_key(table, resource, record, supersede, range?)

    if has_filter?(filter) do
      case get_object(table, pkey) do
        {:ok, {_key, record}} when is_map(record) ->
          with {:ok, record} <- cast_record(record, resource),
               {:ok, [_]} <- filter_matches([record], filter, domain, tenant, actor) do
            retire(table, pkey, resource, record, supersede, range?)
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

        {:ok, _} ->
          {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, filter: filter)}

        {:error, error} ->
          {:error, error}
      end
    else
      retire(table, pkey, resource, record, supersede, range?)
    end
  end

  # A range reaches every version it overlaps. An instant closes only the version given.
  defp retire(table, _pkey, resource, record, {period, %Ash.Range{} = written}, true) do
    with {:ok, periods} <- stored_periods(table, resource, record) do
      primary_key = primary_key(resource, record)

      periods
      |> Map.get(primary_key, [])
      |> Enum.filter(&Ash.Range.intersects?(&1, written))
      |> case do
        [] -> {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, field: period)}
        versions -> close_versions(table, resource, primary_key, period, versions, written)
      end
    end
  end

  defp retire(table, pkey, resource, _record, supersede, _range?),
    do: retire(table, pkey, resource, supersede)

  defp retire(table, pkey, resource, supersede) do
    case {supersede, get_object(table, pkey)} do
      {{period, %Ash.Range{} = written}, {:ok, {_key, stored}}} when is_map(stored) ->
        close_version(table, pkey, stored, resource, period, written)

      # No version holds the instant, so there is none for it to end.
      {{period, %Ash.Range{}}, {:ok, _}} ->
        {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, field: period)}

      _ ->
        with {:ok, _} <- delete_object(table, pkey), do: :ok
    end
  end

  defp close_versions(table, resource, primary_key, period, versions, written) do
    Enum.reduce_while(versions, :ok, fn version, :ok ->
      key = Map.put(primary_key, period, version)

      with {:ok, {_key, stored}} <- get_object(table, key),
           :ok <- close_version(table, key, stored, resource, period, overlap(version, written)) do
        {:cont, :ok}
      else
        error -> {:halt, error}
      end
    end)
  end

  # A destroy ends validity over the period it names. Closing a version at the instant it
  # began leaves nothing to keep, so it goes.
  defp close_version(table, pkey, stored, resource, period, written) do
    prior = Map.get(pkey, period)

    with {:ok, closed} <- close_at(prior, written.lower, resource, period),
         {:ok, resumed} <- resume_after(prior, written, resource, period),
         {:ok, versions} <- carved(resource, pkey, period, stored, [closed, resumed]),
         {:ok, table} <- delete_object(table, pkey),
         {:ok, _} <- put_versions(table, versions) do
      :ok
    end
  end

  defp put_versions(table, []), do: {:ok, table}

  defp put_versions(table, versions), do: put_objects(table, versions)

  # An instant ends validity for good; a range hands back what lies beyond its upper bound.
  defp resume_after(_prior, %Ash.Range{upper: nil}, _resource, _period), do: {:ok, nil}

  defp resume_after(%Ash.Range{} = prior, %Ash.Range{} = written, resource, period) do
    resumed = %{
      prior
      | lower: written.upper,
        bounds: bounds(not Ash.Range.upper_inclusive?(written.bounds), prior.bounds)
    }

    cond do
      not Ash.Range.contains?(prior, written) ->
        {:error,
         Ash.Error.Changes.PeriodOutOfBounds.exception(
           resource: resource,
           field: period,
           period: written,
           within: prior
         )}

      Ash.Range.empty?(resumed) ->
        {:ok, nil}

      true ->
        {:ok, resumed}
    end
  end

  # The part of `written` that `version` holds.
  defp overlap(version, written) do
    {lower, lower_bounds} =
      if later?(written.lower, version.lower),
        do: {written.lower, written.bounds},
        else: {version.lower, version.bounds}

    {upper, upper_bounds} =
      if earlier?(written.upper, version.upper),
        do: {written.upper, written.bounds},
        else: {version.upper, version.bounds}

    %Ash.Range{
      lower: lower,
      upper: upper,
      bounds: bounds(Ash.Range.lower_inclusive?(lower_bounds), upper_bounds)
    }
  end

  # `nil` is unbounded: no lower is later, and no upper is earlier.
  defp later?(_left, nil), do: true
  defp later?(nil, _right), do: false
  defp later?(left, right), do: Comp.greater_than?(left, right)

  defp earlier?(_left, nil), do: true
  defp earlier?(nil, _right), do: false
  defp earlier?(left, right), do: Comp.less_than?(left, right)

  defp bounds(lower_inclusive?, prior_bounds) do
    case {lower_inclusive?, Ash.Range.upper_inclusive?(prior_bounds)} do
      {true, true} -> :"[]"
      {true, false} -> :"[)"
      {false, true} -> :"(]"
      {false, false} -> :"()"
    end
  end

  defp carved(resource, pkey, period, stored, ranges) do
    ranges
    |> Enum.reject(&is_nil/1)
    |> Enum.reduce_while({:ok, []}, fn range, {:ok, acc} ->
      case version(resource, pkey, period, stored, range) do
        {:ok, version} -> {:cont, {:ok, [version | acc]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
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

    query =
      Map.update!(query, :filter, fn filter ->
        if is_nil(changeset.filter) do
          filter
        else
          filter = filter || %Ash.Filter{resource: changeset.resource}
          Ash.Filter.add_to_filter!(filter, changeset.filter)
        end
      end)

    query
    |> run_query(resource)
    |> case do
      {:ok, results} ->
        Enum.reduce_while(results, acc, fn result, acc ->
          if touches_portion?(resource, result, changeset.as_of) do
            transact_write(resource, query.tenant, false, fn ->
              case still_matching(resource, changeset, query, result) do
                {:ok, nil} ->
                  :skipped

                {:ok, current} ->
                  result_changeset = %{changeset | data: current}

                  with {:ok, result} <- update(query.resource, result_changeset, nil, true),
                       do: {:ok, Ash.Actions.Helpers.Bulk.put_metadata(result, result_changeset)}

                {:error, error} ->
                  {:error, error}
              end
            end)
            |> case do
              :skipped ->
                {:cont, acc}

              {:ok, result} ->
                case acc do
                  :ok -> {:cont, :ok}
                  {:ok, results} -> {:cont, {:ok, [result | results]}}
                end

              {:error, error} ->
                {:halt, {:error, error}}
            end
          else
            {:cont, acc}
          end
        end)

      {:error, error} ->
        {:error, error}
    end
    |> case do
      :ok -> :ok
      {:ok, results} -> {:ok, first_versions(resource, Enum.reverse(results))}
      {:error, error} -> {:error, error}
    end
  end

  # A range can carve several versions of one record; the write returns the first of them.
  defp first_versions(resource, results) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        results

      attribute ->
        firsts =
          results
          |> Enum.group_by(&primary_key(resource, &1))
          |> Map.new(fn {key, versions} ->
            {key, Enum.min_by(versions, &Map.get(&1, attribute).lower, Comp)}
          end)

        Enum.filter(results, &(Map.fetch!(firsts, primary_key(resource, &1)) == &1))
    end
  end

  @doc false
  @impl true
  def update(resource, changeset, pkey \\ nil, from_bulk? \\ false) do
    transact_write(resource, changeset.tenant, false, fn ->
      supersede = supersession(resource, changeset)
      range? = match?(%Ash.Range{}, changeset.as_of)

      with :ok <- lock_record(resource, changeset.tenant, changeset.data),
           {:ok, table} <- written_table(resource, changeset.tenant, false),
           pkey = pkey || holding_key(table, resource, changeset.data, supersede, range?),
           _ <- if(!from_bulk?, do: log_update(resource, pkey, changeset)),
           {:ok, record} <- do_update(table, pkey, changeset, resource, supersede),
           {:ok, record} <- cast_record(record, resource),
           record <- retain_fields(record, changeset) do
        new_pkey = pkey_map(resource, record)

        # A supersession has already retired the old key, so it must not be destroyed.
        if is_nil(supersede) && new_pkey != pkey do
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
        # Nothing has ever been written for this tenant, so the record cannot exist.
        :no_table ->
          {:error,
           Ash.Error.Changes.StaleRecord.exception(
             resource: resource,
             filter: changeset.filter
           )}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  # `nil` writes in place: no period, or a period with no now to supersede at.
  defp supersession(resource, changeset) do
    with period when not is_nil(period) <- Ash.Resource.Info.temporal_attribute(resource),
         {:ok, written} <- Ash.Temporal.write_period(resource, changeset.as_of) do
      {period, written}
    else
      _ -> nil
    end
  end

  # An instant write reaches the version holding its instant, as a SQL layer's `@> as_of`
  # match does, rather than only the version the caller holds: that one may have been
  # superseded since it was read. A range reaches the versions it overlaps on its own.
  defp holding_key(table, resource, record, {period, %Ash.Range{lower: instant}}, false) do
    pkey = pkey_map(resource, record)

    with false <- holds?(table, pkey, period, instant),
         {:ok, periods} <- stored_periods(table, resource, record),
         %Ash.Range{} = holding <-
           periods
           |> Map.get(primary_key(resource, record), [])
           |> Enum.find(&(match?(%Ash.Range{}, &1) and Ash.Range.contains?(&1, instant))) do
      Map.put(pkey, period, holding)
    else
      _ -> pkey
    end
  end

  defp holding_key(_table, resource, record, _supersede, _range?), do: pkey_map(resource, record)

  defp holds?(table, pkey, period, instant) do
    case Map.get(pkey, period) do
      %Ash.Range{} = held ->
        Ash.Range.contains?(held, instant) and
          match?({:ok, {_key, stored}} when is_map(stored), get_object(table, pkey))

      _ ->
        false
    end
  end

  defp retain_fields(%struct{} = record, %{data: %struct{} = data}) do
    attributes = Enum.map(Ash.Resource.Info.attributes(struct), & &1.name)
    take = [:__metadata__, :__meta__ | attributes]
    Map.merge(data, Map.take(record, take))
  end

  defp retain_fields(record, _) do
    record
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
    |> key_fields()
    |> Enum.into(%{}, fn attr ->
      {attr, Map.get(data, attr)}
    end)
  end

  defp key_fields(resource) do
    primary_key = Ash.Resource.Info.primary_key(resource)

    case Ash.Resource.Info.temporal_attribute(resource) do
      nil -> primary_key
      period -> primary_key ++ [period]
    end
  end

  defp do_update(table, pkey, changeset, resource, supersede) do
    attributes = resource |> Ash.Resource.Info.attributes()
    %{domain: domain, tenant: tenant, filter: changeset_filter} = changeset
    actor = changeset.context[:private][:actor]

    case dump_to_native(changeset.attributes, attributes) do
      {:ok, casted} ->
        case get_object(table, pkey) do
          {:ok, {_key, record}} when is_map(record) ->
            with {:ok, casted_record} <- cast_record(record, resource),
                 {:ok, [casted_record]} <-
                   filter_matches([casted_record], changeset_filter, domain, tenant, actor),
                 {:ok, dumped_atomics} <-
                   (case changeset.atomics do
                      empty when empty in [nil, []] ->
                        {:ok, %{}}

                      atomics ->
                        with {:ok, atomics} <-
                               make_atomics(atomics, resource, domain, casted_record),
                             do: dump_atomics(atomics, attributes)
                    end),
                 data = record |> Map.merge(casted) |> Map.merge(dumped_atomics),
                 {:ok, written} <-
                   if(Ash.DataLayer.Ets.Info.transaction_group(resource),
                     do: cast_record(data, resource),
                     else: {:ok, nil}
                   ),
                 :ok <-
                   enforce_identities(resource, casted_record, written, identity_opts(changeset)) do
              write_version(table, pkey, record, data, resource, supersede)
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
               resource: resource,
               filter: changeset_filter
             )}

          other ->
            other
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp write_version(table, pkey, _prior, data, _resource, nil), do: put_data(table, pkey, data)

  # No transaction here, so the delete goes first: a reader sees the record absent, never
  # twice.
  defp write_version(table, pkey, prior_data, data, resource, {period, written}) do
    prior = Map.get(pkey, period)
    written = clip_to_prior(prior, written)

    with {:ok, closed} <- close_at(prior, written.lower, resource, period),
         {:ok, resumed} <- resume_after(prior, written, resource, period),
         {:ok, opening} <- open_at(prior, written, resource, period),
         {:ok, {_key, opened_data} = opened} <-
           version(resource, pkey, period, data, opening),
         {:ok, kept} <- carved(resource, pkey, period, prior_data, [closed, resumed]),
         {:ok, table} <- delete_object(table, pkey),
         {:ok, _table} <- put_versions(table, kept ++ [opened]) do
      {:ok, opened_data}
    end
  end

  # A primary-key-only match reaches every version; one the portion misses is left as it was.
  defp touches_portion?(resource, result, %Ash.Range{} = as_of) do
    case Ash.Resource.Info.temporal_attribute(resource) do
      nil ->
        true

      attribute ->
        case Map.get(result, attribute) do
          %Ash.Range{} = prior -> Ash.Range.intersects?(prior, as_of)
          _ -> true
        end
    end
  end

  defp touches_portion?(_resource, _result, _as_of), do: true

  # A version holding only part of a written period takes the part it holds, clipped here.
  defp clip_to_prior(%Ash.Range{lower: prior_lower}, written) when is_nil(prior_lower) do
    written
  end

  defp clip_to_prior(%Ash.Range{} = prior, written) do
    lower =
      if not is_nil(written.lower) and Comp.less_than?(written.lower, prior.lower) do
        prior.lower
      else
        written.lower
      end

    upper =
      case {prior.upper, written.upper} do
        {nil, w} -> w
        {p, nil} -> p
        {p, w} -> if Comp.less_than?(p, w), do: p, else: w
      end

    %{written | lower: lower, upper: upper}
  end

  # An unbounded written upper leaves the prior's in place, so a bare instant is unchanged.
  defp open_at(prior, %Ash.Range{lower: lower, upper: nil}, _resource, _period),
    do: {:ok, %{prior | lower: lower}}

  defp open_at(%Ash.Range{} = prior, %Ash.Range{} = written, resource, period) do
    opening = %{prior | lower: written.lower, upper: written.upper, bounds: written.bounds}

    if Ash.Range.contains?(prior, opening) do
      {:ok, opening}
    else
      {:error,
       Ash.Error.Changes.PeriodOutOfBounds.exception(
         resource: resource,
         field: period,
         period: opening,
         within: prior
       )}
    end
  end

  # An instant the version does not hold cannot split it. `nil` drops a half holding none.
  # Nothing lies before an unbounded lower, so nothing is kept.
  defp close_at(%Ash.Range{}, nil, _resource, _period), do: {:ok, nil}

  defp close_at(%Ash.Range{} = prior, as_of, resource, period) do
    closed = %{prior | upper: as_of}

    cond do
      not Ash.Range.contains?(prior, as_of) ->
        {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, field: period)}

      Ash.Range.empty?(closed) ->
        {:ok, nil}

      true ->
        {:ok, closed}
    end
  end

  defp close_at(_prior, _as_of, resource, period) do
    {:error, Ash.Error.Changes.StaleRecord.exception(resource: resource, field: period)}
  end

  # Cast in the key, dumped in the data, as every other write leaves it.
  defp version(resource, pkey, period, data, range) do
    attribute = Ash.Resource.Info.temporal_period(resource)

    case Ash.Type.dump_to_native(attribute.type, range, attribute.constraints) do
      {:ok, dumped} -> {:ok, {%{pkey | period => range}, Map.put(data, period, dumped)}}
      :error -> {:error, "could not store the period #{inspect(range)}"}
      {:error, error} -> {:error, error}
    end
  end

  defp put_data(table, pkey, data) do
    case put_objects(table, {pkey, data}) do
      {:ok, _set} ->
        {:ok, data}

      error ->
        error
    end
  end

  defp dump_atomics(atomics, attributes) do
    attributes
    |> Enum.filter(&Map.has_key?(atomics, &1.name))
    |> Enum.reduce_while({:ok, %{}}, fn attribute, {:ok, acc} ->
      case Ash.Type.cast_input(
             attribute.type,
             Map.get(atomics, attribute.name),
             attribute.constraints
           ) do
        {:ok, value} ->
          {:cont, {:ok, Map.put(acc, attribute.name, value)}}

        _ ->
          {:halt,
           {:error,
            Ash.Error.Changes.InvalidAttribute.exception(
              field: attribute.name,
              message: "is invalid"
            )}}
      end
    end)
    |> case do
      {:ok, casted} -> dump_to_native(casted, attributes)
      {:error, error} -> {:error, error}
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

  # Returns `:no_table` when `create?` is false and no table has been created yet.
  # Callers must treat that as "the table exists but is empty", rather than creating
  # one, so that reads against unknown tenants allocate nothing. See `table_name/3`.
  defp wrap_or_create_table(resource, tenant, create?) do
    tenant =
      if Ash.Resource.Info.multitenancy_strategy(resource) == :context do
        tenant
      end

    if Ash.DataLayer.Ets.Info.private?(resource) do
      configured_table = Ash.DataLayer.Ets.Info.table(resource)

      case Process.get({:ash_ets_table, configured_table, tenant}) do
        nil ->
          if create? do
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
          else
            :no_table
          end

        tab ->
          {:ok, tab}
      end
    else
      TableManager.start(resource, tenant, create?)
    end
  end

  defp log_bulk_create(resource, stream, options) do
    Logger.debug(
      "#{bulk_create_operation(options, stream)} #{inspect(resource)}:\n\n#{Enum.map_join(stream, "\n", &format_changes(&1, from_bulk_create?: true))}"
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
      if query.filter && query.filter.expression != nil do
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
      if query.filter && query.filter.expression != nil do
        " matching filter `#{inspect(query.filter.expression)}`"
      else
        ""
      end

    Logger.debug("""
    ETS: Updating #{limit}#{inspect(resource)}#{offset}#{sort}#{filter}:

    #{format_changes(changeset)}
    """)

    :ok
  end

  defp log_create(resource, changeset) do
    Logger.debug("""
    Creating #{inspect(resource)}:

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
    "Updating #{inspect(resource)} #{pkey}:

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
                if Ash.Expr.expr?(nested) do
                  nested
                else
                  # halt so `Ash.Filter.map` doesn't descend into non-expression
                  # values (it enumerates structs, which may not be Enumerable);
                  # `truncate_unless_expr` recurses into containers itself
                  {:halt, truncate_unless_expr(nested, inspect_opts)}
                end
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

        is_struct(nested) ->
          nested

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
end
