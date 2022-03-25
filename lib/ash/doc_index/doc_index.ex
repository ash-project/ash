defmodule Ash.DocIndex do
  @moduledoc "A module for configuring how a library is rendered in ash_hq"
  @type extension :: %{
          optional(:module) => module,
          optional(:target) => String.t(),
          optional(:default_for_target?) => boolean,
          optional(:name) => String.t(),
          optional(:type) => String.t()
        }

  @callback extensions() :: list(extension())
  @callback for_library() :: String.t()
end
