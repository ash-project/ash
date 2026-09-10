# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.CanRead do
  @moduledoc """
  This check passes if the actor could read the related record(s) at the given relationship path.

  The related resource's read action is authorized for the current actor, and the resulting
  authorization filter (combined with the action's own filter) is applied to the related records
  using `exists/2`. See `Ash.Policy.Check.Builtins.can_read/2` for more.
  """
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")

    if opts[:action] do
      "actor can read record.#{path} via #{inspect(opts[:action])}"
    else
      "actor can read record.#{path}"
    end
  end

  @impl true
  def filter(actor, authorizer, opts) do
    path = List.wrap(opts[:relationship_path])
    resource = opts[:resource] || authorizer.resource

    if path == [] do
      raise ArgumentError, "`can_read` requires a non-empty relationship path"
    end

    relationship = last_relationship!(resource, path)
    destination = relationship.destination
    action = read_action!(relationship, opts[:action])
    domain = Ash.Resource.Info.domain(destination) || authorizer.domain
    tenant = subject_tenant(authorizer)

    with_cycle_detection({destination, action.name}, resource, path, fn ->
      query = build_query(destination, action, actor, tenant, domain)

      case related_filter(query, actor) do
        false ->
          false

        expr ->
          Ash.Query.Exists.new(path, expr)
      end
    end)
  end

  defp build_query(destination, action, actor, tenant, domain) do
    query =
      Ash.Query.for_read(destination, action.name, %{},
        actor: actor,
        tenant: tenant,
        domain: domain
      )

    if query.valid? do
      query
    else
      raise ArgumentError, """
      Could not build a query for `#{inspect(destination)}.#{action.name}` in a `can_read` check.

      `can_read` builds the related read action's query with no arguments, so read actions
      with required arguments cannot be used.

      Errors:

      #{Exception.message(Ash.Error.to_error_class(query.errors))}
      """
    end
  end

  # Mirrors how read actions authorize their own query, but without running it. The returned
  # query's filter contains the action's filter combined with the authorization filter.
  defp related_filter(query, actor) do
    case Ash.can(query, actor,
           alter_source?: true,
           run_queries?: false,
           no_check?: true,
           pre_flight?: false,
           maybe_is: false
         ) do
      {:ok, true, %Ash.Query{filter: nil}} ->
        true

      {:ok, true, %Ash.Query{filter: %Ash.Filter{expression: expression}}} ->
        expression

      {:ok, false} ->
        false

      {:ok, false, _error} ->
        false

      {:error, error} ->
        if must_pass_strict_check?(error) do
          raise ArgumentError, """
          Cannot use `can_read` for `#{inspect(query.resource)}.#{query.action.name}`.

          The policies for that action require runtime checks, which cannot be expressed as a
          filter. `can_read` can only be used when the related resource's read action can be fully
          authorized with filter checks (`expr/1`, `relates_to_actor_via/2`, etc.) and simple
          checks (`actor_attribute_equals/2`, `actor_present/0`, etc.).
          """
        else
          raise Ash.Error.to_ash_error(error)
        end
    end
  end

  defp must_pass_strict_check?(%Ash.Error.Forbidden.Policy{must_pass_strict_check?: true}),
    do: true

  defp must_pass_strict_check?(%{errors: errors}) when is_list(errors),
    do: Enum.any?(errors, &must_pass_strict_check?/1)

  defp must_pass_strict_check?(_), do: false

  defp read_action!(relationship, nil) do
    cond do
      relationship.read_action ->
        Ash.Resource.Info.action(relationship.destination, relationship.read_action)

      action = Ash.Resource.Info.primary_action(relationship.destination, :read) ->
        action

      true ->
        raise ArgumentError, """
        No read action specified for `can_read` check on relationship `#{relationship.name}`,
        and `#{inspect(relationship.destination)}` has no primary read action.

        Specify one with `can_read(#{inspect(relationship.name)}, action: :action_name)`
        """
    end
  end

  defp read_action!(relationship, action_name) do
    case Ash.Resource.Info.action(relationship.destination, action_name, :read) do
      nil ->
        raise ArgumentError,
              "No such read action `#{inspect(action_name)}` on `#{inspect(relationship.destination)}`, required in `can_read` check"

      action ->
        action
    end
  end

  defp last_relationship!(resource, [rel_key]) do
    relationship!(resource, rel_key)
  end

  defp last_relationship!(resource, [rel_key | rest]) do
    last_relationship!(relationship!(resource, rel_key).destination, rest)
  end

  defp relationship!(resource, rel_key) do
    Ash.Resource.Info.relationship(resource, rel_key) ||
      raise ArgumentError,
            "No such relationship `#{inspect(rel_key)}` for `#{inspect(resource)}`, required in `can_read` check"
  end

  defp subject_tenant(%{subject: %{tenant: tenant}}), do: tenant
  defp subject_tenant(_), do: nil

  @stack_key {__MODULE__, :stack}

  defp with_cycle_detection(key, resource, path, fun) do
    stack = Process.get(@stack_key, [])

    if key in stack do
      {destination, action_name} = key

      raise ArgumentError, """
      Detected a cycle in `can_read` checks.

      Authorizing `#{inspect(destination)}.#{action_name}` requires evaluating a `can_read`
      check on `#{inspect(resource)}` via `#{Enum.join(path, ".")}`, which in turn requires
      authorizing `#{inspect(destination)}.#{action_name}` again.

      Stack: #{inspect(Enum.reverse([key | stack]))}
      """
    end

    Process.put(@stack_key, [key | stack])

    try do
      fun.()
    after
      case stack do
        [] -> Process.delete(@stack_key)
        stack -> Process.put(@stack_key, stack)
      end
    end
  end
end
