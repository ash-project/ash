defmodule Ash.Error.Load.NoSuchRelationship do
  @moduledoc "Used when attempting to load a relationship that does not exist"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :relationship, :load_path], class: :invalid

  def message(%{resource: resource, relationship: relationship, load_path: load_path}) do
    if load_path == [] do
      "No such relationship #{inspect(resource)}.#{relationship}"
    else
      "No such relationship #{inspect(resource)}.#{relationship} at #{Enum.join(load_path, ".")}"
    end
  end
end
