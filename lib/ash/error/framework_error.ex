defmodule Ash.Error.FrameworkError do
  @moduledoc "Raised when an internal framework error occurs. These are always bugs."
  defexception [:message]
end
