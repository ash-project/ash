defprotocol Ash.ToTenant do
  @moduledoc """
  Converts a value to a tenant. To add this to a resource, implement the protocol like so:application

  What this should do is entirely dependent on how you've set up your tenants. This example assumes
  that you want the tenant to be `org_\#{organization_id}`, but it could also be something like
  `organization.schema`.

  ```elixir
  defmodule MyApp.Organization do
    use Ash.Resource, ...

    ...

    defimpl Ash.ToTenant do
      def to_tenant(%{id: id}), do: "org_\#{id}"
    end
  end
  ```
  """

  @type t :: term()

  @spec to_tenant(Ash.Resource.t(), t) :: term()
  def to_tenant(resource, value)
end

defimpl Ash.ToTenant, for: BitString do
  def to_tenant(_resource, value), do: value
end

defimpl Ash.ToTenant, for: Atom do
  def to_tenant(_resource, nil), do: nil
  def to_tenant(_resource, value), do: value
end

defimpl Ash.ToTenant, for: Integer do
  def to_tenant(_resource, value), do: value
end
