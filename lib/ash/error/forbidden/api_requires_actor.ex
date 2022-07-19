defmodule Ash.Error.Forbidden.ApiRequiresActor do
  @moduledoc "Used when an api that has `require_actor? true` is provided no actor"
  use Ash.Error.Exception

  def_ash_error([:api], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "actor_required_by_api"

    def message(%{api: api}) do
      "The api #{inspect(api)} requires that an actor is provided at all times and none was provided."
    end
  end
end
