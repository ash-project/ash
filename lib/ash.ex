defmodule Ash do
  @moduledoc """
  General purpose tools for working with Ash and Ash resources.
  """

  for {function, arity} <- Ash.Domain.Functions.functions() do
    if function == :load do
      def load({:ok, result}, load) do
        load(result, load)
      end

      def load({:error, error}, _), do: {:error, error}

      def load([], _), do: {:ok, []}
      def load(nil, _), do: {:ok, nil}

      def load(%page_struct{results: []} = page, _)
          when page_struct in [Ash.Page.Keyset, Ash.Page.Offset] do
        {:ok, page}
      end
    end

    if function == :load! do
      def load!({:ok, result}, load) do
        {:ok, load!(result, load)}
      end

      def load!({:error, error}, _), do: raise(Ash.Error.to_error_class(error))
      def load!([], _), do: []
      def load!(nil, _), do: nil

      def load!(%page_struct{results: []} = page, _)
          when page_struct in [Ash.Page.Keyset, Ash.Page.Offset] do
        page
      end
    end

    args = Macro.generate_arguments(arity, __MODULE__)

    docs_arity =
      if function in Ash.Domain.Functions.no_opts_functions() do
        arity
      else
        arity + 1
      end

    @doc "Calls `c:Ash.Domain.#{function}/#{docs_arity}` on the resource's configured domain. See those callback docs for more."
    def unquote(function)(unquote_splicing(args)) do
      resource =
        Ash.Domain.GlobalInterface.resource_from_args!(unquote(function), unquote(arity), [
          unquote_splicing(args)
        ])

      domain = Ash.Resource.Info.domain(resource)

      if !domain do
        Ash.Domain.GlobalInterface.raise_no_domain_error!(
          resource,
          unquote(function),
          unquote(arity)
        )
      end

      apply(domain, unquote(function), [unquote_splicing(args)])
    end

    unless function in Ash.Domain.Functions.no_opts_functions() do
      args = Macro.generate_arguments(arity + 1, __MODULE__)

      if function == :load! do
        def load!({:ok, result}, load, opts) do
          {:ok, load(result, load, opts)}
        end

        def load!({:error, error}, _, _), do: raise(Ash.Error.to_error_class(error))

        def load!(nil, _, _), do: nil
        def load!([], _, _), do: []

        def load!(%page_struct{results: []} = page, _, _)
            when page_struct in [Ash.Page.Keyset, Ash.Page.Offset] do
          page
        end
      end

      if function == :load do
        def load({:ok, result}, load, opts) do
          load(result, load, opts)
        end

        def load({:error, error}, _, _), do: {:error, error}
        def load([], _, _), do: {:ok, []}
        def load(nil, _, _), do: {:ok, nil}

        def load(%page_struct{results: []} = page, _, _)
            when page_struct in [Ash.Page.Keyset, Ash.Page.Offset] do
          {:ok, page}
        end
      end

      @doc "Calls `c:Ash.Domain.#{function}/#{arity + 1}` on the resource's configured domain. See those callback docs for more."
      def unquote(function)(unquote_splicing(args)) do
        resource =
          Ash.Domain.GlobalInterface.resource_from_args!(unquote(function), unquote(arity), [
            unquote_splicing(args)
          ])

        domain = Ash.Resource.Info.domain(resource)

        if !domain do
          Ash.Domain.GlobalInterface.raise_no_domain_error!(
            resource,
            unquote(function),
            unquote(arity)
          )
        end

        apply(domain, unquote(function), [unquote_splicing(args)])
      end
    end
  end

  @doc """
  Converts a context map to opts to be passed into an action.
  """

  def context_to_opts(map, add_to \\ []) when is_map(map) do
    add_to
    |> add_if_present(map, :actor)
    |> add_if_present(map, :authorize?)
    |> add_if_present(map, :tracer)
    |> add_if_present(map, :tenant)
  end

  defp add_if_present(opts, map, key) do
    case Map.fetch(map, key) do
      {:ok, value} -> Keyword.put(opts, key, value)
      :error -> opts
    end
  end

  @doc deprecated: "See `Ash.ProcessHelpers`. This alias will be removed in 3.0"
  defdelegate get_context_for_transfer(opts \\ []), to: Ash.ProcessHelpers
  @doc deprecated: "See `Ash.ProcessHelpers`. This alias will be removed in 3.0"
  defdelegate transfer_context(term, opts \\ []), to: Ash.ProcessHelpers

  @doc deprecated: """
       Sets context into the process dictionary that is used for all changesets and queries.
       """
  @spec set_context(map) :: :ok
  def set_context(map) do
    Process.put(:ash_context, map)

    :ok
  end

  @doc deprecated: """
       Deep merges context into the process dictionary that is used for all changesets and queries.
       """
  @spec merge_context(map) :: :ok
  def merge_context(map) do
    update_context(&Ash.Helpers.deep_merge_maps(&1, map))

    :ok
  end

  @doc deprecated: """
       Updates the context into the process dictionary that is used for all changesets and queries.
       """
  @spec update_context((map -> map)) :: :ok
  def update_context(fun) do
    context = Process.get(:ash_context, %{})
    set_context(fun.(context))

    :ok
  end

  @doc deprecated: """
       Sets actor into the process dictionary that is used for all changesets and queries.
       """
  @spec set_actor(map) :: :ok
  def set_actor(map) do
    Process.put(:ash_actor, {:actor, map})

    :ok
  end

  @doc deprecated: """
       Sets authorize? into the process dictionary that is used for all changesets and queries.
       """
  @spec set_authorize?(map) :: :ok
  def set_authorize?(map) do
    Process.put(:ash_authorize?, {:authorize?, map})

    :ok
  end

  @doc deprecated: """
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

  @doc deprecated: """
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

  @doc deprecated: """
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

  @doc deprecated: """
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

  @doc deprecated: """
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

  @doc deprecated: """
       Sets tenant into the process dictionary that is used for all changesets and queries.
       """
  @spec set_tenant(term()) :: :ok
  def set_tenant(tenant) do
    Process.put(:ash_tenant, {:tenant, tenant})

    :ok
  end

  @doc deprecated: """
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

  @doc deprecated: """
       Gets the current context from the process dictionary
       """
  @spec get_context() :: term()
  def get_context do
    Process.get(:ash_context, %{}) || %{}
  end
end
