defmodule Ash do
  @moduledoc """
  General purpose tools for working with Ash.

  Currently only contains setters/getters for process context.
  """

  @doc """
  Converts a context map to opts to be passed into an action.
  """
  def context_to_opts(map, add_to \\ []) when is_map(map) do
    add_to
    |> add_if_present(map, :actor)
    |> add_if_present(map, :authorize?)
    |> add_if_present(map, :tracer)
  end

  defp add_if_present(opts, map, key) do
    case Map.fetch(map, key) do
      {:ok, value} -> Keyword.put(opts, key, value)
      :error -> opts
    end
  end

  @doc """
  Gets all of the ash context so it can be set into a new process.

  Use `transfer_context/1` in the new process to set the context.
  """
  @spec get_context_for_transfer(opts :: Keyword.t()) :: term
  def get_context_for_transfer(opts \\ []) do
    context = Ash.get_context()
    actor = Process.get(:ash_actor)
    authorize? = Process.get(:ash_authorize?)
    tenant = Process.get(:ash_tenant)
    tracer = Process.get(:ash_tracer)

    tracer_context =
      opts[:tracer]
      |> List.wrap()
      |> Enum.concat(List.wrap(tracer))
      |> Map.new(fn tracer ->
        {tracer, Ash.Tracer.get_span_context(tracer)}
      end)

    %{
      context: context,
      actor: actor,
      tenant: tenant,
      authorize?: authorize?,
      tracer: tracer,
      tracer_context: tracer_context
    }
  end

  @spec transfer_context(term, opts :: Keyword.t()) :: :ok
  def transfer_context(
        %{
          context: context,
          actor: actor,
          tenant: tenant,
          authorize?: authorize?,
          tracer: tracer,
          tracer_context: tracer_context
        },
        _opts \\ []
      ) do
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

    case authorize? do
      {:authorize?, authorize?} ->
        Ash.set_authorize?(authorize?)

      _ ->
        :ok
    end

    Ash.set_tracer(tracer)

    Enum.each(tracer_context || %{}, fn {tracer, tracer_context} ->
      Ash.Tracer.set_span_context(tracer, tracer_context)
    end)

    Ash.set_context(context)
  end

  @doc """
  Sets context into the process dictionary that is used for all changesets and queries.

  In Ash 3.0, this will be updated to deep merge
  """
  @spec set_context(map) :: :ok
  def set_context(map) do
    Process.put(:ash_context, map)

    :ok
  end

  @doc """
  Deep merges context into the process dictionary that is used for all changesets and queries.
  """
  @spec merge_context(map) :: :ok
  def merge_context(map) do
    update_context(&Ash.Helpers.deep_merge_maps(&1, map))

    :ok
  end

  @doc """
  Updates the context into the process dictionary that is used for all changesets and queries.
  """
  @spec update_context((map -> map)) :: :ok
  def update_context(fun) do
    context = Process.get(:ash_context, %{})
    set_context(fun.(context))

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
  Sets authorize? into the process dictionary that is used for all changesets and queries.
  """
  @spec set_authorize?(map) :: :ok
  def set_authorize?(map) do
    Process.put(:ash_authorize?, {:authorize?, map})

    :ok
  end

  @doc """
  Sets the tracer into the process dictionary that will be used to trace requests
  """
  @spec set_tracer(module | list(module)) :: :ok
  def set_tracer(module) do
    case Process.get(:ash_tracer, module) do
      nil -> Process.put(:ash_tracer, module)
      tracer -> Process.put(:ash_tracer, Enum.uniq(List.wrap(tracer) ++ List.wrap(module)))
    end

    :ok
  end

  @doc """
  Removes a tracer from the process dictionary.
  """
  @spec remove_tracer(module | list(module)) :: :ok
  def remove_tracer(module) do
    case Process.get(:ash_tracer, module) do
      nil -> :ok
      tracer -> Process.put(:ash_tracer, List.wrap(tracer) -- List.wrap(module))
    end

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
  Gets the current tracer
  """
  @spec get_tracer() :: term()
  def get_tracer do
    case Process.get(:ash_tracer) do
      {:tracer, value} ->
        value

      _ ->
        Application.get_env(:ash, :tracer)
    end
  end

  @doc """
  Gets the current authorize? from the process dictionary
  """
  @spec get_authorize?() :: term()
  def get_authorize? do
    case Process.get(:ash_authorize?) do
      {:authorize?, value} ->
        value

      _ ->
        nil
    end
  end

  @doc """
  Sets tenant into the process dictionary that is used for all changesets and queries.
  """
  @spec set_tenant(String.t()) :: :ok
  def set_tenant(tenant) do
    Process.put(:ash_tenant, {:tenant, tenant})

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
