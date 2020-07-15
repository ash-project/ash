defmodule Ash.Error.SideLoad.NoSuchRelationship do
  @moduledoc "Used when attempting to side load a relationship that does not exist"
  use Ash.Error

  def_ash_error([:resource, :relationship, :side_load_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_side_load_relationship"

    def class(_), do: :invalid

    def message(%{resource: resource, relationship: relationship, side_load_path: side_load_path}) do
      if side_load_path == [] do
        "No such relationship #{inspect(resource)}.#{relationship}"
      else
        "No such relationship #{inspect(resource)}.#{relationship} at #{
          Enum.join(side_load_path, ".")
        }"
      end
    end

    def stacktrace(_), do: nil
  end
end
