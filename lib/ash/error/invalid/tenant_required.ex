defmodule Ash.Error.Invalid.TenantRequired do
  @moduledoc "Used when a tenant is not specified"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    "Queries against the #{inspect(resource)} resource require a tenant to be specified"
  end
end
