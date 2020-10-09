defmodule Ash.Error.Dsl.DslError do
  @moduledoc "Used when a DSL is incorrectly configured."
  defexception [:module, :message, :path]

  def message(%{module: module, message: message, path: nil}) do
    "[#{normalize_module_name(module)}]\n #{message}"
  end

  def message(%{module: module, message: message, path: dsl_path}) do
    dsl_path = Enum.join(dsl_path, " -> ")
    "[#{normalize_module_name(module)}]\n #{dsl_path}:\n  #{message}"
  end

  defp normalize_module_name(module) do
    inspect(module)
  end
end
