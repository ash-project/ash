defmodule Ash.Policy.Test.Rbac.Checks.RoleChecks do
  @moduledoc false

  @behaviour Ash.Policy.Check
  require Ash.Query

  # Description of role hierarchy/permissions granted
  @role_inheritance [:admin, :member, :viewer]

  @permission_grants %{
    admin: [:create, :read, :update, :destroy],
    member: [:create, :read, :update],
    viewer: [:read]
  }

  def describe(_opts) do
    "A description"
  end

  ## Helper to use in resources
  def can?(resource), do: {__MODULE__, [resource: resource]}

  ## Helper to use in resources
  def has_permission?(resource, permission) do
    roles =
      permission
      |> roles_that_can()
      |> all_higher_roles()

    {__MODULE__, [resource: resource, roles: roles]}
  end

  ## Helper to use in resources
  def has_role?(resource, role_or_roles) do
    {__MODULE__, [resource: resource, roles: all_higher_roles(role_or_roles)]}
  end

  # Says "I need to know these field values to do my work"
  def strict_check_context(_opts) do
    [:query, :action]
  end

  # This would be a spot for us to pre-empt doing any filtering/request work
  # We could use this to say "we already kow the answer" For example,
  # if the actor had a `super_admin` flag enabled on then, we could return
  # {:ok, true} to say "whatever role you're asking about, they have it"
  # (don't implement that this way, use a bypass check, its just an example)
  def strict_check(_, _, _) do
    {:ok, :unknown}
  end

  # Describes the type of check
  def type, do: :filter

  # Returns a filter statement over the data
  def auto_filter(actor, authorizer, []) do
    # :create | :update | :destroy | :read
    action_type = authorizer.action.type

    Ash.Query.expr(
      memberships.role in ^roles_that_can(action_type) and memberships.user_id == ^actor.id
    )
  end

  def auto_filter(actor, authorizer, opts) do
    # We're asking for specific roles
    if opts[:roles] do
      Ash.Query.expr(
        organization.memberships.role in ^opts[:roles] and
          organization.memberships.user_id == ^actor.id and
          organization.memberships.resource == ^opts[:resource] and
          id == organization.memberships.resource_id
      )
    else
      # We're asking if they have the permission corresponding to the action
      # :create | :update | :destroy | :read
      action_type = authorizer.action.type

      Ash.Query.expr(
        organization.memberships.role in ^roles_that_can(action_type) and
          organization.memberships.user_id == ^actor.id and
          organization.memberships.resource == ^opts[:resource] and
          id == organization.memberships.resource_id
      )
    end
  end

  defp roles_that_can(permission) do
    @permission_grants
    |> Enum.filter(fn {_key, val} ->
      permission in val
    end)
    |> Enum.map(&elem(&1, 0))
  end

  defp all_higher_roles(role_or_roles) do
    role_or_roles
    |> List.wrap()
    |> Enum.flat_map(fn role ->
      Enum.take_while(@role_inheritance, &(&1 != role))
    end)
    |> Enum.uniq()
  end
end
