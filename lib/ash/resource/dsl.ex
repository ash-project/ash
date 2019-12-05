defmodule Ash.Resource.DSL do
  @moduledoc """
  The entrypoint for the Ash DSL documentation and interface.

  Available DSL sections:

  * `actions` - `Ash.Resource.Actions`
  * `attributes` - `Ash.Resource.Attributes`
  * `relationships` - `Ash.Resource.Relationships`

  See the relevant module documentation. To use sections in your resource:

  ```elixir
  defmodule MyModule do
    use Ash.Resource, name: "foos", type: "foo"

    actions do
      ...
      # see actions documentation
    end

    attributes do
      ...
      # see attributes documentation
    end

    relationships do
      ...
      # see relationships documentation
    end
  end
  ```
  """

  defmacro __using__(_) do
    quote do
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Resource.Attributes, only: [attributes: 1]
      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end
end
