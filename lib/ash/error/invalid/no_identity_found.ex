defmodule Ash.Error.Invalid.NoIdentityFound do
  @moduledoc "Used when an identity name is used that does not reference identity on the resource"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :identity], class: :invalid

  def message(%{resource: resource, identity: identity}) do
    """
    Identity #{inspect(identity)} does not exist on resource #{inspect(resource)}.

    Available identities are:

    #{identities(resource)}
    """
  end

  defp identities(resource) do
    resource
    |> Ash.Resource.Info.identities()
    |> Enum.map_join("\n", &"* #{inspect(&1.name)} - #{inspect(&1.keys)}")
  end
end
