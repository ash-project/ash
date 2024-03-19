defmodule Ash.Error.Forbidden.DomainRequiresAuthorization do
  @moduledoc "Used when a domain that has `authorize :always` is provided authorize?: false"
  use Ash.Error.Exception

  use Splode.Error, fields: [:domain], class: :forbidden

  def message(%{domain: domain}) do
    "The domain #{inspect(domain)} requires that authorization is run, but `authorize?: false` was given."
  end
end
