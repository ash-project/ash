defmodule Ash.Domain.GlobalInterface do
  @moduledoc false

  @doc false
  def raise_no_domain_error!(resource, function, arity) do
    raise ArgumentError, """
    No domain configured for resource #{inspect(resource)}.

    To configure one, use the `:domain` option in the resource, for example: `use Ash.Resource, domain: Domain`.

    If the resource is meant to be used with multiple domains (a rare case), include the `:domain` option to `Ash.#{function}/#{arity}`.
    """
  end

  @doc false
  def resource_from_args!(fun, _, [data | _]) when fun in [:load, :load!] do
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

  def resource_from_args!(fun, _, [%resource{} | _]) when fun in [:reload, :reload!] do
    resource
  end

  def resource_from_args!(fun, _, [_, resource | _]) when fun in [:bulk_create, :bulk_create!] do
    resource
  end

  def resource_from_args!(fun, _, [resource | _])
      when fun in [:calculate, :calculate!, :get, :get!] do
    resource
  end

  def resource_from_args!(fun, _, [page | _]) when fun in [:page, :page!] do
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

  def resource_from_args!(fun, _, [query_or_changeset_or_action | _])
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

  def resource_from_args!(fun, _arity, [changeset_or_record | _])
      when fun in [:destroy, :update, :destroy!, :update!] do
    case changeset_or_record do
      %struct{resource: resource} when struct in [Ash.Changeset] ->
        resource

      %resource{} ->
        resource

      other ->
        raise """
        Could not determine resource. Expected a changeset, query, action input, or resource.

        Got: #{inspect(other)}
        """
    end
  end

  def resource_from_args!(fun, _arity, [changeset_or_query | _])
      when fun in [
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

  def resource_from_args!(fun, arity, _args) do
    raise ArgumentError, "Could not determine resource from arguments to `Ash.#{fun}/#{arity}`"
  end
end
