defmodule Ash.Error.Query.NotFound do
  @moduledoc "Used when an entity that not exist is referenced"
  use Ash.Error.Exception

  def_ash_error([:primary_key, :resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "not_found"

    def class(_), do: :invalid

    def message(%{primary_key: key, resource: _resource}) do
      "record with #{id_string(key)} not found"
    end

    def stacktrace(_), do: nil

    defp id_string(map) do
      Enum.map_join(map, " | ", fn {key, value} ->
        "#{key}: #{inspect(value)}"
      end)
    end
  end
end
