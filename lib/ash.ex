defmodule Ash do
  @moduledoc """
  General purpose tools for working with Ash.

  Currently only contains setters/getters for process context.
  """

  @doc """
  Gets all of the ash context so it can be set into a new process.

  Use `transfer_context/1` in the new process to set the context.
  """
  @spec get_context_for_transfer() :: term
  def get_context_for_transfer do
    context = Ash.get_context()
    actor = Process.get(:ash_actor)
    tenant = Process.get(:tenant)

    %{context: context, actor: actor, tenant: tenant}
  end

  @spec transfer_context(term) :: :ok
  def transfer_context(%{context: context, actor: actor, tenant: tenant}) do
    case actor do
      {:actor, actor} ->
        Ash.set_actor(actor)

      _ ->
        :ok
    end

    case tenant do
      {:tenant, tenant} ->
        Ash.set_tenant(tenant)

      _ ->
        :ok
    end

    Ash.set_context(context)
  end

  @doc """
  Sets context into the process dictionary that is used for all changesets and queries.
  """
  @spec set_context(map) :: :ok
  def set_context(map) do
    Process.put(:ash_context, map)

    :ok
  end

  @doc """
  Sets actor into the process dictionary that is used for all changesets and queries.
  """
  @spec set_actor(map) :: :ok
  def set_actor(map) do
    Process.put(:ash_actor, {:actor, map})

    :ok
  end

  @doc """
  Gets the current actor from the process dictionary
  """
  @spec get_actor() :: term()
  def get_actor do
    case Process.get(:ash_actor) do
      {:actor, value} ->
        value

      _ ->
        nil
    end
  end

  @doc """
  Sets tenant into the process dictionary that is used for all changesets and queries.
  """
  @spec set_tenant(map) :: :ok
  def set_tenant(map) do
    Process.put(:ash_tenant, {:tenant, map})

    :ok
  end

  @doc """
  Gets the current tenant from the process dictionary
  """
  @spec get_tenant() :: term()
  def get_tenant do
    case Process.get(:ash_tenant) do
      {:tenant, value} ->
        value

      _ ->
        nil
    end
  end

  @doc """
  Gets the current context from the process dictionary
  """
  @spec get_context() :: term()
  def get_context do
    Process.get(:ash_context, %{}) || %{}
  end
end
