defmodule Ash.Error.SideLoad.InvalidQuery do
  @moduledoc "Used when an invalid query is provided in a side load"
  use Ash.Error.Exception

  def_ash_error([:query, :side_load_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_side_load_query"

    def class(_), do: :invalid

    def message(%{query: query, side_load_path: side_load_path}) do
      "Invalid query: #{inspect(query)} at #{Enum.join(side_load_path, ".")}"
    end

    def stacktrace(_), do: nil
  end
end
