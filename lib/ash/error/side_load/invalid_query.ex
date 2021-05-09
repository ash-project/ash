defmodule Ash.Error.Load.InvalidQuery do
  @moduledoc "Used when an invalid query is provided in a load"
  use Ash.Error.Exception

  def_ash_error([:load_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_load_query"

    def class(_), do: :invalid

    def message(%{query: query, load_path: load_path}) do
      "Invalid query: #{inspect(query)} at #{Enum.join(load_path, ".")}"
    end

    def stacktrace(_), do: nil
  end
end
