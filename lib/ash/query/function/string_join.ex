defmodule Ash.Query.Function.StringJoin do
  @moduledoc """
  Joins a list of values.

  Ignores `nil` values and concatenates the remaining non-nil values. An optional
  joiner can beprovided.

      string_join(" ", first_name, last_name)

      string_join(item_a, item_b)
  """

  use Ash.Query.Function, name: :string_join

  def args,
    do: [
      [:string, {:array, :string}],
      [{:array, :string}]
    ]

  def evaluate(%{arguments: [joiner, values]}) do
    join(joiner, values)
  end

  def evaluate(%{arguments: [values]}) do
    join("", values)
  end

  defp join(joiner, values) do
    joined =
      values
      |> Enum.reject(&is_nil/1)
      |> Enum.join(joiner)

    {:known, joined}
  end
end
