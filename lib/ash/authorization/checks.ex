defmodule Ash.Authorization.Checks do
  @moduledoc "Built in authorization checks."

  def always(), do: [__always__: true]

  def related_to_user_via(relationship) do
    relationship
    |> List.wrap()
    |> put_nested_relationship()
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
