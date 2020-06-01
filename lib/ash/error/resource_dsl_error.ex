defmodule Ash.Error.ResourceDslError do
  @moduledoc "Raised when a resource's DSL is incorrectly configured."
  defexception [:message, :path, :using]

  def message(%{message: message, path: nil, using: using}) do
    "`use #{inspect(using)}, ...` #{message} "
  end

  def message(%{message: message, path: nil}) do
    message
  end

  def message(%{message: message, path: dsl_path}) do
    dsl_path = Enum.join(dsl_path, " -> ")
    "#{dsl_path}:\n  #{message}"
  end
end
