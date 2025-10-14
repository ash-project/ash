# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.NoIdentityFound do
  @moduledoc "Used when an identity name is used that does not reference identity on the resource"

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
