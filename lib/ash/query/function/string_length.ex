# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringLength do
  @moduledoc """
  Returns the length of a string.

  Without a unit argument, the length is counted in codepoints, unless the
  backwards-compatibility key `config :ash, :default_string_length_count` is set to
  `:mixed`, in which case it is counted in graphemes (see the backwards compatibility
  guide). An optional second argument selects a unit explicitly:

  - `:graphemes` - counts unicode graphemes, i.e `String.length/1`.
  - `:codepoints` - counts unicode codepoints. This matches how most SQL data layers
    count the length of a string.
  - `:bytes` - counts bytes, i.e `byte_size/1`.

  A single grapheme may be made up of an unbounded number of codepoints
  (e.g. a base character followed by many combining marks), so counting graphemes
  places no effective bound on the size of a value. Use `:codepoints` or `:bytes`
  when the length is being used as a limit.

  Data layers cannot count graphemes, so `:graphemes` is only supported where the
  expression is evaluated in Elixir. Note that `string_length/1` is translated to the
  data layer's native length function, which counts codepoints, regardless of the
  configured default.

      string_length(name)
      string_length(name, :bytes)
  """

  use Ash.Query.Function, name: :string_length

  @counts [:graphemes, :codepoints, :bytes]

  def args,
    do: [
      [:string],
      [:ci_string],
      [:string, {:atom, one_of: @counts}],
      [:ci_string, {:atom, one_of: @counts}]
    ]

  def returns, do: [:integer, :integer, :integer, :integer]

  @doc "The supported units for counting string length."
  def counts, do: @counts

  def new([_value, count] = args) when count in @counts do
    {:ok, struct(__MODULE__, arguments: args)}
  end

  def new([_value, count]) do
    {:error,
     "Invalid unit #{inspect(count)} for string_length/2. Expected one of #{inspect(@counts)}"}
  end

  def new(args), do: {:ok, struct(__MODULE__, arguments: args)}

  def evaluate(%{arguments: [value]}) do
    {:known, string_length(value, Ash.Type.String.default_length_count())}
  end

  def evaluate(%{arguments: [value, count]}) do
    {:known, string_length(value, count)}
  end

  @doc """
  Counts the length of a string in the given unit.

  See the module documentation for the supported units.
  """
  @spec string_length(String.t() | Ash.CiString.t(), :graphemes | :codepoints | :bytes) ::
          non_neg_integer()
  def string_length(%Ash.CiString{string: value}, count), do: string_length(value, count)
  def string_length(value, :graphemes) when is_binary(value), do: String.length(value)
  def string_length(value, :bytes) when is_binary(value), do: byte_size(value)

  def string_length(value, :codepoints) when is_binary(value),
    do: codepoints_length(value, 0)

  defp codepoints_length(<<_::utf8, rest::binary>>, acc), do: codepoints_length(rest, acc + 1)
  defp codepoints_length(<<_, rest::binary>>, acc), do: codepoints_length(rest, acc + 1)
  defp codepoints_length(<<>>, acc), do: acc

  def can_return_nil?(%{arguments: [string | _]}) do
    Ash.Expr.can_return_nil?(string)
  end
end
