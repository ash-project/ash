defmodule Ash.Resource.Actions do
  @moduledoc "Types for Ash actions"
  alias Ash.Resource.Actions.{Create, Destroy, Read, Update}

  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type action_type :: :read | :create | :update | :destroy
end
