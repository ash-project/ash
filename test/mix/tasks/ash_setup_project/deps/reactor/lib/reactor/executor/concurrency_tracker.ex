# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.ConcurrencyTracker do
  @moduledoc """
  Manage shared concurrency pools for multiple Reactors.

  When running a Reactor you can pass the `concurrency_key` option, which will
  cause the Reactor to use the specified pool to ensure that the combined
  Reactors never exceed the pool's available concurrency limit.

  This avoids nested Reactors spawning too many workers and thrashing the
  system.

  The process calling `allocate_pool/1` is monitored, and when it terminates
  it's allocation is removed.  Any processes which are using that pool will
  not be able to allocate any new resources.
  """

  use GenServer

  @type pool_key :: reference()

  @type record ::
          {pool_key, concurrency_limit :: pos_integer, available_slots :: non_neg_integer(),
           allocator :: pid}

  @doc false
  @spec start_link(any) :: GenServer.on_start()
  def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc false
  @impl true
  @spec init(any) :: {:ok, atom | :ets.tid()}
  def init(_) do
    table = :ets.new(__MODULE__, ~w[set named_table public]a)
    {:ok, table}
  end

  @doc false
  @impl true
  def handle_cast({:monitor, pid}, table) do
    Process.monitor(pid)
    {:noreply, table}
  end

  @doc false
  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, table) do
    :ets.select_delete(table, [{{:_, :_, :_, :"$1"}, [], [{:==, :"$1", pid}]}])
    {:noreply, table}
  end

  @doc """
  Allocate a new concurrency pool and set the maximum limit.
  """
  @spec allocate_pool(non_neg_integer) :: pool_key
  def allocate_pool(concurrency_limit) do
    key = make_ref()
    caller = self()
    :ets.insert(__MODULE__, {key, concurrency_limit, concurrency_limit, caller})
    GenServer.cast(__MODULE__, {:monitor, caller})
    key
  end

  @doc """
  Release the concurrency pool.

  This deletes the pool, however doesn't affect any processes currently using
  it.  No more resources can be acquired by users of the pool key.
  """
  @spec release_pool(pool_key) :: :ok
  def release_pool(pool_key) do
    :ets.delete(__MODULE__, pool_key)
    :ok
  end

  @doc """
  Release concurrency allocation back to the pool.
  """
  @spec release(pool_key, how_many :: pos_integer) :: :ok | :error
  def release(key, how_many \\ 1) do
    # generated using:
    #
    # :ets.fun2ms(fn {key, concurrency_limit, concurrency_available, owner}
    #                when key == :key and concurrency_available + 1 <= concurrency_limit ->
    #   {key, concurrency_limit, concurrency_available + 1, owner}
    # end)
    #
    # and replacing `:key` with the provided key.

    Enum.reduce_while(1..how_many, :ok, fn _, :ok ->
      case :ets.select_replace(__MODULE__, [
             {{:"$1", :"$2", :"$3", :"$4"},
              [{:andalso, {:==, :"$1", key}, {:"=<", {:+, :"$3", 1}, :"$2"}}],
              [{{:"$1", :"$2", {:+, :"$3", 1}, :"$4"}}]}
           ]) do
        0 -> {:halt, :error}
        1 -> {:cont, :ok}
      end
    end)
  end

  @doc """
  Attempt to acquire a number of concurrency allocations from the pool.

  Returns `{:ok, n}` where `n` was the number of slots that were actually
  allocated.  It's important to note that whilst you may request `16` slots, if
  there is only `3` available, then this function will return `{:ok, 3}` and you
  must abide by it.

  It is possible for this function to return `{:ok, 0}` if there is no slots
  available.
  """
  @spec acquire(pool_key, how_many :: pos_integer()) :: {:ok, non_neg_integer()}
  def acquire(key, how_many \\ 1) do
    # generated using:
    #
    # :ets.fun2ms(fn {key, concurrency_limit, concurrency_available, owner}
    #                when key == :key and concurrency_available - 1 >= 0 ->
    #   {key, concurrency_limit, concurrency_available - 1, owner}
    # end)
    #
    # and replacing `:key` with the provided key.

    Enum.reduce_while(1..how_many, {:ok, 0}, fn _, {:ok, n} ->
      case :ets.select_replace(__MODULE__, [
             {{:"$1", :"$2", :"$3", :"$4"},
              [{:andalso, {:==, :"$1", key}, {:>=, {:-, :"$3", 1}, 0}}],
              [{{:"$1", :"$2", {:-, :"$3", 1}, :"$4"}}]}
           ]) do
        0 -> {:halt, {:ok, n}}
        1 -> {:cont, {:ok, n + 1}}
      end
    end)
  end

  @doc """
  Report the available and maximum concurrency for a pool.
  """
  @spec status(pool_key) :: {:ok, available, limit} | {:error, any}
        when available: non_neg_integer(), limit: pos_integer()
  def status(key) do
    __MODULE__
    |> :ets.lookup(key)
    |> case do
      [{_, limit, available, _}] -> {:ok, available, limit}
      [] -> {:error, "Unknown concurrency pool"}
    end
  end
end
