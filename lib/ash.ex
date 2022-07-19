defmodule Ash do
  @moduledoc """
  General purpose tools for working with Ash.

  Currently only contains setters/getters for process context.
  """

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
