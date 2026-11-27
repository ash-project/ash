# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defprotocol Ash.ToTenant do
  @moduledoc """
  Converts a value to a tenant.

  To add this to a resource, implement the protocol like so:

  ```elixir
  defmodule MyApp.Organization do
    use Ash.Resource, ...

    ...

    defimpl Ash.ToTenant do
      def to_tenant(%{id: id}, _resource), do: "org_\#{id}"
    end
  end
  ```

  What this should do is entirely dependent on how you've set up your tenants. This example assumes
  that you want the tenant to be `org_\#{organization_id}`, but it could also be something like
  `organization.schema`.
  """

  @type t :: term()

  @doc """
  Returns the tenant identifier for the given value and target resource.

  Use the `resource` argument when the same value needs to produce different identifiers
  for different resources. The identifier should match the value that the resource
  expects for its configured multitenancy strategy.

  ## Examples

      iex> Ash.ToTenant.to_tenant("org_42", MyApp.Post)
      "org_42"

      iex> Ash.ToTenant.to_tenant(42, MyApp.Post)
      42
  """
  @spec to_tenant(t, Ash.Resource.t()) :: term()
  def to_tenant(value, resource)
end

defimpl Ash.ToTenant, for: BitString do
  def to_tenant(value, _resource), do: value
end

defimpl Ash.ToTenant, for: Atom do
  def to_tenant(nil, _resource), do: nil
  def to_tenant(value, _resource), do: value
end

defimpl Ash.ToTenant, for: Integer do
  def to_tenant(value, _resource), do: value
end
