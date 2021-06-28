defmodule Ash.Error.Query.ReadActionRequiresActor do
  @moduledoc "Used when an actor is referenced in a filter template, but no actor exists"
  use Ash.Error.Exception

  def_ash_error([], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "actor_required"

    def message(_error) do
      "actor is required"
    end
  end
end
