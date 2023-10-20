defmodule Ash.Api.GlobalInterface do
  @moduledoc false
  for {function, arity} <- Ash.Api.Functions.functions() do
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
      if function in Ash.Api.Functions.no_opts_functions() do
        arity
      else
        arity + 1
      end

    @doc "Calls `c:Ash.Api.#{function}/#{docs_arity}` on the resource's configured api. See those callback docs for more."
    def unquote(function)(unquote_splicing(args)) do
      resource = resource_from_args!(unquote(function), unquote(arity), [unquote_splicing(args)])

      api = Ash.Resource.Info.api(resource)

      if !api do
        raise_no_api_error!(resource, unquote(function), unquote(arity))
      end

      apply(api, unquote(function), [unquote_splicing(args)])
    end

    unless function in Ash.Api.Functions.no_opts_functions() do
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

      @doc "Calls `c:Ash.Api.#{function}/#{arity + 1}` on the resource's configured api. See those callback docs for more."
      def unquote(function)(unquote_splicing(args)) do
        resource =
          resource_from_args!(unquote(function), unquote(arity), [unquote_splicing(args)])

        api = Ash.Resource.Info.api(resource)

        if !api do
          raise_no_api_error!(resource, unquote(function), unquote(arity))
        end

        apply(api, unquote(function), [unquote_splicing(args)])
      end
    end
  end

  defp raise_no_api_error!(resource, function, arity) do
    raise ArgumentError, """
    No api configured for resource #{inspect(resource)}.

    To configure one, use `api: MyApi` in the resource's options, for example: `use Ash.Resource, api: YourApi`.

    If the resource is meant to be used with multiple Apis (a rare case), call that api direction instead of using `Ash.#{function}/#{arity}`.
    """
  end

  defp resource_from_args!(fun, _, [data | _]) when fun in [:load, :load!] do
    case data do
      %struct{rerun: {%Ash.Query{resource: resource}, _}}
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] ->
        resource

      %struct{rerun: {resource, _}}
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] and is_atom(resource) ->
        resource

      %resource{} ->
        resource

      [%resource{} | _] ->
        resource

      {:ok, %resource{}} ->
        resource

      {:ok, [%resource{} | _]} ->
        resource
    end
  end

  defp resource_from_args!(:reload, _, [%resource{} | _]) do
    resource
  end

  defp resource_from_args!(fun, _, [_, resource | _]) when fun in [:bulk_create, :bulk_create!] do
    resource
  end

  defp resource_from_args!(fun, _, [resource | _]) when fun in [:calculate, :calculate!] do
    resource
  end

  defp resource_from_args!(fun, _, [page | _]) when fun in [:page, :page!] do
    case page do
      %struct{rerun: {%Ash.Query{resource: resource}, _}}
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] ->
        resource

      %struct{rerun: {resource, _}}
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] and is_atom(resource) ->
        resource

      other ->
        raise """
        Could not determine resource. Expected an `Ash.Page.Keyset` or `Ash.Page.Offset`.

        Got: #{inspect(other)}
        """
    end
  end

  defp resource_from_args!(fun, _, [query_or_changeset_or_action | _])
       when fun in [:can, :can?] do
    case query_or_changeset_or_action do
      %struct{resource: resource} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
        resource

      {resource, _} ->
        resource

      {resource, _, _} ->
        resource
    end
  end

  defp resource_from_args!(fun, _arity, [changeset_or_query | _])
       when fun in [
              :destroy,
              :update,
              :destroy!,
              :update!,
              :read,
              :read!,
              :stream,
              :stream!,
              :create,
              :create!,
              :run_action,
              :run_action!,
              :read_one,
              :read_one!,
              :get,
              :count,
              :count!,
              :first,
              :first!,
              :sum,
              :sum!,
              :min,
              :min!,
              :max,
              :max!,
              :avg,
              :avg!,
              :exists,
              :exists?,
              :list,
              :list!,
              :aggregate,
              :aggregate!
            ] do
    case changeset_or_query do
      %struct{resource: resource} when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
        resource

      resource when is_atom(resource) and not is_nil(resource) ->
        resource

      other ->
        raise """
        Could not determine resource. Expected a changeset, query, action input, or resource.

        Got: #{inspect(other)}
        """
    end
  end

  defp resource_from_args!(fun, arity, _args) do
    raise ArgumentError, "Could not determine resource from arguments to `Ash.#{fun}/#{arity}`"
  end
end
