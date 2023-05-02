defmodule Ash.Resource.Actions do
  @moduledoc "Types for Ash actions"
  alias Ash.Resource.Actions.{Action, Create, Destroy, Read, Update}

  @type action :: Action.t() | Create.t() | Read.t() | Update.t() | Destroy.t()
  @type action_type :: :action | :read | :create | :update | :destroy
end
