defmodule Ash.Error.Invalid.ResourceNotAllowed do
  @moduledoc "Used when a resource or alias is provided that cannot be used with the given domain"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :domain], class: :invalid

  def message(%{domain: domain, resource: resource}) do
    "Resource `#{inspect(resource)}` is not accepted by #{inspect(domain)}"
  end
end
