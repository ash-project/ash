defmodule Ash.Error.Invalid.MultipleResults do
  @moduledoc "Used when multiple requests with the same path are passed to the internal engine"
  use Ash.Error.Exception

  def_ash_error([:count, :query, :at_least?], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "multiple_results"

    def message(%{count: count, query: nil, at_least?: at_least?}) do
      "expected at most one result but got #{at_least(at_least?)}#{count}"
    end

    def message(%{count: count, query: query, at_least?: at_least?}) do
      """
      expected at most one result but got #{at_least(at_least?)}#{count} in query:

      #{inspect(query)}
      """
    end

    defp at_least(true), do: "at least "
    defp at_least(_), do: ""
  end
end
