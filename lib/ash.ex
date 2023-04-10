defmodule Ash do
  @moduledoc """
  General purpose tools for working with Ash.

  Currently only contains setters/getters for process context.
  """

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
      if opts[:tracer] do
        {:tracer_context, opts[:tracer].get_span_context()}
      else
        case tracer do
          {:tracer, tracer} ->
            {:tracer_context, tracer.get_span_context()}

          _ ->
            nil
        end
      end

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
        opts \\ []
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

    if opts[:tracer] do
      case tracer_context do
        {:tracer_context, tracer_context} ->
          opts[:tracer].set_span_context(tracer_context)

        _ ->
          :ok
      end
    else
      case tracer do
        {:tracer, tracer} ->
          Ash.set_tracer(tracer)

          case tracer_context do
            {:tracer_context, tracer_context} ->
              tracer.set_span_context(tracer_context)

            _ ->
              :ok
          end

        _ ->
          tracer = Application.get_env(:ash, :tracer) || opts[:tracer]

          if tracer do
            case tracer_context do
              {:tracer_context, tracer_context} ->
                tracer.set_span_context(tracer_context)

              _ ->
                :ok
            end
          end

          :ok
      end
    end

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
  @spec set_tracer(module) :: :ok
  def set_tracer(module) do
    Process.put(:ash_tracer, module)

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
