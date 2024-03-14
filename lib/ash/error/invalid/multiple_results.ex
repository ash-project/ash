defmodule Ash.Error.Invalid.MultipleResults do
  @moduledoc "Used when multiple requests with the same path are passed to the internal engine"
  use Ash.Error.Exception

  use Splode.Error, fields: [:count, :at_least?, :query], class: :invalid

  def splode_message(%{count: count, query: nil, at_least?: at_least?}) do
    "expected at most one result but got #{at_least(at_least?)}#{count}.

    Please ensure your action is configured with an appropriate filter to ensure a single result is returned."
  end

  def splode_message(%{count: count, query: query, at_least?: at_least?}) do
    """
    expected at most one result but got #{at_least(at_least?)}#{count} in query:

    #{inspect(query)}
    """
  end

  defp at_least(true), do: "at least "
  defp at_least(_), do: ""
end
