defmodule Ash.Routes do
  def get(resource, id) do
    index(resource) <> "/" <> to_string(id)
  end

  def index(resource) do
    "/" <> Ash.name(resource)
  end
end
