defmodule Ash.Resource.Actions.ManagedRelationship do
  @moduledoc "Represents a relationship managed by an argument to an action"
  defstruct [:name, :relationship, :authorize?, :on_create, :on_destroy, :on_update]

  @type t :: %__MODULE__{}
end
