defmodule Ash.Error.Forbidden.DomainRequiresActor do
  @moduledoc "Used when an domain that has `require_actor? true` is provided no actor"
  use Ash.Error.Exception

  use Splode.Error, fields: [:domain], class: :forbidden

  def splode_message(%{domain: domain}) do
    "The domain #{inspect(domain)} requires that an actor is provided at all times and none was provided."
  end
end
