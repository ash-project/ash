defmodule Ash.Query.Function.StringJoin do
  @moduledoc """
  Joins a list of values.

  Ignores `nil` values and concatenates the remaining non-nil values. An optional
  joiner can be provided.

      string_join([first_name, last_name], " ")

      string_join([item_a, item_b])
  """

  use Ash.Query.Function, name: :string_join
  alias Ash.CiString

  def args,
    do: [
      [{:array, :string}],
      [{:array, :string}, :string],
      [{:array, :string}, :ci_string],
      [{:array, :ci_string}],
      [{:array, :ci_string}, :ci_string],
      [{:array, :ci_string}, :ci_string]
    ]

  def evaluate(%{arguments: [values, joiner]}) do
    join(values, joiner)
  end

  def evaluate(%{arguments: [values]}) do
    join(values, "")
  end

  defp join(values, joiner) do
    joined = normalize_and_join(values, joiner)

    if has_ci_string?([joiner | values]) do
      {:known, CiString.new(joined)}
    else
      {:known, joined}
    end
  end

  defp normalize_and_join(values, joiner) do
    joiner = joiner |> normalize()

    values
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&normalize/1)
    |> Enum.join(joiner)
  end

  defp normalize(%CiString{} = ci_string), do: ci_string |> CiString.value()
  defp normalize(string) when is_binary(string), do: string

  defp has_ci_string?(values) do
    values
    |> Enum.any?(fn
      %CiString{} -> true
      _ -> false
    end)
  end
end
