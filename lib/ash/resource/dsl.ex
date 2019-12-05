defmodule Ash.Resource.DSL do
  @moduledoc """
  The entrypoint for the Ash DSL documentation and interface.

  Available DSL sections:

  * `actions` - `Ash.Resource.Actions`
  * `attributes` - `Ash.Resource.Attributes`
  * `relationships` - `Ash.Resource.Relationships`
  """

  defmacro __using__(_) do
    quote do
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Resource.Attributes, only: [attributes: 1]
      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end
end
