defmodule Ash.Query.Function.Contains do
  @moduledoc """
  Returns true if the first string contains the second.

  Case insensitive strings are accounted for on either side.

     contains("foo", "fo")
     true

     contains(%Ash.CiString{:string "foo"}, "FoO")
     true

     contains("foo", %Ash.CiString{:string "FOO"})
     true
  """
  use Ash.Query.Function, name: :contains

  alias Ash.CiString

  def args,
    do: [
      [:string, :string],
      [:ci_string, :ci_string],
      [:string, :ci_string],
      [:ci_string, :string]
    ]

  def evaluate(%{arguments: [nil, _]}), do: false
  def evaluate(%{arguments: [_, nil]}), do: false

  def evaluate(%{arguments: [%CiString{} = left, %CiString{} = right]}) do
    {:known, String.contains?(CiString.value(left), CiString.value(right))}
  end

  def evaluate(%{arguments: [left, %Ash.CiString{} = right]}) when is_binary(left) do
    {:known,
     left
     |> String.downcase()
     |> String.contains?(CiString.value(right))}
  end

  def evaluate(%{arguments: [%Ash.CiString{} = left, right]})
      when is_binary(right) do
    {:known, String.contains?(Ash.CiString.value(left), String.downcase(right))}
  end

  def evaluate(%{arguments: [left, right]})
      when is_binary(left) and is_binary(right) do
    {:known, String.contains?(left, right)}
  end

  def evaluate(_other) do
    :unknown
  end
end
