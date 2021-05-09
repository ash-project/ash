defmodule Ash.Error.Load.NoSuchRelationship do
  @moduledoc "Used when attempting to load a relationship that does not exist"
  use Ash.Error.Exception

  def_ash_error([:resource, :relationship, :load_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_load_relationship"

    def class(_), do: :invalid

    def message(%{resource: resource, relationship: relationship, load_path: load_path}) do
      if load_path == [] do
        "No such relationship #{inspect(resource)}.#{relationship}"
      else
        "No such relationship #{inspect(resource)}.#{relationship} at #{
          Enum.join(load_path, ".")
        }"
      end
    end

    def stacktrace(_), do: nil
  end
end
