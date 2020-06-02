defmodule Ash.Error.FrameworkError do
  @moduledoc "Used when an internal framework error occurs. These are always bugs."
  defexception [:message]
end
