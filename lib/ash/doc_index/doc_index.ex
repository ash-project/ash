defmodule Ash.DocIndex do
  @moduledoc "A module for configuring how a library is rendered in ash_hq"
  @type extension :: %{
          optional(:module) => module,
          optional(:target) => String.t(),
          optional(:default_for_target?) => boolean,
          :name => String.t(),
          :type => String.t()
        }

  @type guide :: %{
          name: String.t(),
          text: String.t()
        }

  @callback extensions() :: list(extension())
  @callback for_library() :: String.t()
  @callback guides() :: list(guide())
end
