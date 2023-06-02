defmodule Ash.Error.Query.NotFound do
  @moduledoc "Used when an entity that not exist is referenced"
  use Ash.Error.Exception

  def_ash_error([:primary_key, :resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "not_found"

    def message(%{primary_key: nil, resource: _resource}) do
      "record not found"
    end

    def message(%{primary_key: key, resource: _resource}) do
      "record with #{id_string(key)} not found"
    end

    defp id_string(%Ash.Filter{} = filter) do
      inspect(filter)
    end

    defp id_string(map) when is_map(map) do
      if Map.has_key?(map, :__struct__) do
        inspect(map)
      else
        Enum.map_join(map, " | ", fn {key, value} ->
          "#{key}: #{inspect(value)}"
        end)
      end
    end

    defp id_string(other) do
      inspect(other)
    end
  end
end
