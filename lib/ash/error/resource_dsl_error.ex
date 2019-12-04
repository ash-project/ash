defmodule Ash.Error.ResourceDslError do
  defexception [:message, :path, :option, :resource]

  def message(%{message: message, path: dsl_path, option: nil, resource: resource}) do
    dsl_path = Enum.join(dsl_path, "->")
    "#{inspect(resource)}: #{message} at #{dsl_path}"
  end

  def message(%{message: message, path: dsl_path, option: option, resource: resource}) do
    dsl_path = Enum.join(dsl_path, "->")

    "#{inspect(resource)}: option #{option} at #{dsl_path} #{message}"
  end
end
