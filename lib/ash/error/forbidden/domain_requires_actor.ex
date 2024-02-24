defmodule Ash.Error.Forbidden.DomainRequiresActor do
  @moduledoc "Used when an domain that has `require_actor? true` is provided no actor"
  use Ash.Error.Exception

  def_ash_error([:domain], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "actor_required_by_domain"

    def message(%{domain: domain}) do
      "The domain #{inspect(domain)} requires that an actor is provided at all times and none was provided."
    end
  end
end
