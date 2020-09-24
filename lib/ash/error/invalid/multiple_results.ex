defmodule Ash.Error.Invalid.MultipleResults do
  @moduledoc "Used when multiple requests with the same path are passed to the internal engine"
  use Ash.Error

  def_ash_error([:count, :query], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "multiple_results"

    def message(%{count: count, query: query}) do
      """
      expected at most one result but got #{count} in query:

      #{inspect(query)}
      """
    end
  end
end
