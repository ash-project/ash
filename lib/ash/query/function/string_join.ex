defmodule Ash.Query.Function.StringJoin do
  @moduledoc """
  Joins a list of values.

  Ignores `nil` values and concatenates the remaining non-nil values. An optional
  joiner can be provided.

      string_join([first_name, last_name], " ")

      string_join([item_a, item_b])
  """

  use Ash.Query.Function, name: :string_join

  def args,
    do: [
      [{:array, :string}],
      [{:array, :string}, :string]
    ]

  def evaluate(%{arguments: [values, joiner]}) do
    join(values, joiner)
  end

  def evaluate(%{arguments: [values]}) do
    join(values)
  end

  defp join(values, joiner \\ "") do
    joined =
      values
      |> Enum.reject(&is_nil/1)
      |> Enum.join(joiner)

    {:known, joined}
  end
end
