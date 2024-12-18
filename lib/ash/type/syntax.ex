defmodule Ash.Type.Syntax do
  @moduledoc """
  The syntax for the type sigil in Ash
  """

  import NimbleParsec

  whitespace =
    times(choice([string(" "), string("\t"), string("\n"), string("\r")]), min: 1)

  foo =
    string("foo")
    |> tag(:foo)

  bar =
    string("bar")
    |> tag(:bar)

  concrete_type =
    choice([foo, bar])

  defcombinatorp :union,
                 concrete_type
                 |> ignore(optional(whitespace))
                 |> ignore(string("|"))
                 |> ignore(optional(whitespace))
                 |> concat(parsec(:full_type))
                 |> ignore(optional(whitespace))
                 |> tag(:union)

  # defcombinator :type, choice([parsec(:union), string("foo")])
  #

  defcombinatorp :full_type,
                 ignore(optional(whitespace))
                 |> choice([parsec(:union), concrete_type])
                 |> ignore(optional(whitespace))

  defparsec :type, parsec(:full_type) |> map({:to_type, []})

  defp to_type(v) do
    IO.inspect(v)
    :nothing
  end

  # def sigil_TYPE(text, options) do
  #   text
  #   |> String.split(" | ")
  # end
end
