defmodule Ash.Error.ApiDslError do
  defexception [:message, :path, :option, :using]

  def message(%{message: message, path: nil, option: option, using: using}) do
    "`use #{inspect(using)}, ...` #{option} #{message}"
  end

  def message(%{message: message, path: nil, option: option}) do
    "#{option} #{message}"
  end

  def message(%{message: message, path: dsl_path, option: nil}) do
    dsl_path = Enum.join(dsl_path, " -> ")
    "#{message} at #{dsl_path}"
  end

  def message(%{message: message, path: dsl_path, option: option}) do
    dsl_path = Enum.join(dsl_path, " -> ")

    "option #{option} at #{dsl_path} #{message}"
  end
end
