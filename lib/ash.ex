defmodule Ash do
  def resources() do
    Application.get_env(:ash, :resources) || []
  end

  def actions(resource) do
    resource.actions()
  end

  def attributes(resource) do
    resource.attributes()
  end

  def name(resource) do
    resource.name()
  end

  def type(resource) do
    resource.type()
  end
end
