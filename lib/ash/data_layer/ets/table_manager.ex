defmodule Ash.DataLayer.Ets.TableManager do
  @moduledoc """
  A GenServer which "owns" the ETS table(s) at run time.
  """
  use GenServer

  @ets_options [ordered: true, read_concurrency: true]

  @doc "Starts the storage for a given resource/tenant"
  @spec start(Ash.Resource.t(), String.t() | nil) :: {:ok, ETS.Set.t()} | {:error, any}
  def start(resource, tenant) do
    table = table_name_for_resource_and_tenant(resource, tenant)

    if Ash.DataLayer.Ets.Info.private?(resource) do
      do_wrap_existing(resource, table)
    else
      name = process_name_for_resource_and_tenant(resource, tenant)

      case GenServer.start(__MODULE__, {resource, table}, name: name) do
        {:error, {:already_started, _pid}} ->
          ETS.Set.wrap_existing(table)

        {:error, error} ->
          {:error, error}

        _ ->
          ETS.Set.wrap_existing(table)
      end
    end
  end

  @doc "Stops the storage for a given resource/tenant (deleting all of the data)"
  @spec stop(Ash.Resource.t(), String.t() | nil) :: :ok
  def stop(resource, tenant) do
    if Ash.DataLayer.Ets.Info.private?(resource) do
      case Process.get({:ash_ets_table, resource, tenant}) do
        nil ->
          :ok

        table ->
          ETS.Set.delete(table)
          :ok
      end
    else
      resource
      |> process_name_for_resource_and_tenant(tenant)
      |> Process.whereis()
      |> case do
        pid when is_pid(pid) ->
          Process.exit(pid, :shutdown)
          :ok

        _ ->
          :ok
      end
    end
  end

  @doc """
  Ensure that the ETS table exists for the given resource/tenant combination.

  Creates one if it does not exist.
  """
  @spec wrap_or_create_table(Ash.Resource.t(), String.t() | nil) ::
          {:ok, ETS.Set.t()} | {:error, any}
  def wrap_or_create_table(resource, tenant) do
    if Ash.DataLayer.Ets.Info.private?(resource) do
      table_name = table_name_for_resource_and_tenant(resource, tenant)

      with nil <- Process.get({:ash_ets_table, table_name, tenant}),
           {:ok, table} <- ETS.Set.new(Keyword.put(@ets_options, :protection, :private)) do
        Process.put({:ash_ets_table, table_name, tenant}, table)
        {:ok, table}
      else
        {:error, reason} -> {:error, reason}
        table -> {:ok, table}
      end
    else
      start(resource, tenant)
    end
  end

  defp process_name_for_resource_and_tenant(resource, tenant) do
    resource
    |> table_name_for_resource_and_tenant(tenant)
    |> Module.concat(TableManager)
  end

  defp table_name_for_resource_and_tenant(resource, nil),
    do: Ash.DataLayer.Ets.Info.table(resource)

  # sobelow_skip ["DOS.StringToAtom"]
  defp table_name_for_resource_and_tenant(resource, tenant) do
    resource
    |> Ash.DataLayer.Ets.Info.table()
    |> to_string()
    |> Kernel.<>(tenant)
    |> String.to_atom()
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
    with {:error, :table_not_found} <- ETS.Set.wrap_existing(table),
         {:error, :table_already_exists} <-
           ETS.Set.new(Keyword.merge(@ets_options, name: table, protection: :public)) do
      ETS.Set.wrap_existing(table)
    else
      {:ok, table} -> {:ok, table}
      {:error, reason} -> {:error, reason}
    end
  end
end
