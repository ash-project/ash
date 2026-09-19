# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defprotocol Ash.ToAncestorTenants do
  @moduledoc """
  Derives the ancestor tenant values from a tenant, for resources using the
  `ancestor_attributes` multitenancy option.

  Returns the ancestors' attribute values ordered from broadest to narrowest,
  matching the resource's `ancestor_attributes`. For example, with
  department-level tenancy inside an organization:

  ```elixir
  defmodule MyApp.Department do
    use Ash.Resource, ...

    defimpl Ash.ToTenant do
      def to_tenant(department, _resource), do: department.id
    end

    defimpl Ash.ToAncestorTenants do
      def to_ancestor_tenants(department, _resource), do: [department.organization_id]
    end
  end
  ```

  A deeper hierarchy just returns more ancestors, e.g
  `[team.organization_id, team.department_id]` for `ancestor_attributes [:organization_id, :department_id]`
  with team-level tenancy.

  The default implementation returns `[]`, so tenants without an
  implementation are unaffected. When a resource configures
  `ancestor_attributes` and the ancestors don't line up with it, the operation
  raises instead of silently dropping the ancestor filters.
  """

  @fallback_to_any true

  @type t :: term()

  @spec to_ancestor_tenants(t, Ash.Resource.t()) :: list(term)
  def to_ancestor_tenants(value, resource)
end

defimpl Ash.ToAncestorTenants, for: Any do
  def to_ancestor_tenants(_value, _resource), do: []
end
