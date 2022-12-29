defmodule Ash.Error.Invalid.TenantRequired do
  @moduledoc "Used when a tenant is not specified"
  use Ash.Error.Exception

  def_ash_error([:resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "tenant_required"

    def message(%{resource: resource}) do
      "Queries against the #{inspect(resource)} resource require a tenant to be specified"
    end
  end
end
