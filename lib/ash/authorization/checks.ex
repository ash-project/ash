defmodule Ash.Authorization.Checks do
  @moduledoc "Built in authorization checks."

  def always() do
    [always: true]
  end

  def related_to_user_via(relationship) do
    filter =
      relationship
      |> List.wrap()
      |> put_nested_relationship()

    [filter: filter]
  end

  defp put_nested_relationship([rel | rest]) do
    [
      {rel, put_nested_relationship(rest)}
    ]
  end

  defp put_nested_relationship([]) do
    [id: :__user_id__]
  end
end
