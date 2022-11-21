defmodule Ash.Error.Load.InvalidQuery do
  @moduledoc "Used when an invalid query is provided in a load"
  use Ash.Error.Exception

  def_ash_error([:load_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_load_query"

    def class(_), do: :invalid

    def message(%{query: query, load_path: load_path}) do
      errors_by_path =
        query.errors
        |> Enum.group_by(&List.wrap(&1.path))
        |> Enum.sort_by(&Enum.count(elem(&1, 0)))
        |> Enum.map(fn {path, error} ->
          {List.wrap(load_path) ++ path, error}
        end)

      "Invalid query\n" <>
        Enum.map_join(errors_by_path, "\n", fn {key, errors} ->
          Enum.map_join(errors, "\n", &"* #{inspect(key)} - #{Exception.message(&1)}")
        end)
    end

    def stacktrace(_), do: nil
  end
end
