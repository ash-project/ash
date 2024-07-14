defmodule Ash.Query.Function.Fragment do
  @moduledoc """
  Adds the given interval from the current time in UTC.

  For example:
     expires_at < from_now(7, :day)

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :fragment, no_inspect?: true

  # Varargs is special, and should only be used in rare circumstances (like this one)
  # no type casting or help can be provided for these functions.
  def args, do: :var_args

  def returns, do: [:any]

  def private?, do: true

  def new([fragment | rest] = args) when is_binary(fragment) do
    fragment = "(" <> fragment <> ")"
    split = split_fragment(fragment)

    if Enum.count(split, &(&1 == :slot)) != length(rest) do
      {:error,
       "fragment(...) expects extra arguments in the same amount of question marks in string. " <>
         "It received #{Enum.count(split, &(&1 == :slot))} extra argument(s) but expected #{length(rest)}"}
    else
      {:ok, %__MODULE__{arguments: merge_fragment(split, rest), extra: %{original: args}}}
    end
  end

  def new([fragment | _] = args) when is_function(fragment) do
    {:ok, %__MODULE__{arguments: args}}
  end

  def new([{m, f, a} | _] = args) when is_atom(m) and is_atom(f) and is_list(a) do
    {:ok, %__MODULE__{arguments: args}}
  end

  def new(_) do
    {:error, "First argument to `fragment` must be a string, a one argument function, or an MFA."}
  end

  def evaluate(%{arguments: [function | rest]}) when is_function(function) do
    {:known, apply(function, rest)}
  end

  def evaluate(%{arguments: [{m, f, a} | rest]}) do
    {:known, apply(m, f, a ++ rest)}
  end

  def evaluate(_), do: :unknown

  def casted_new([fragment | rest]) do
    split = split_fragment(fragment)

    if Enum.count(split, &(&1 == :slot)) != length(rest) do
      {:error,
       "fragment(...) expects extra arguments in the same amount of question marks in string. " <>
         "It received #{Enum.count(split, &(&1 == :slot))} extra argument(s) but expected #{length(rest)}"}
    else
      {:ok, %__MODULE__{arguments: merge_fragment(split, rest, :casted_expr)}}
    end
  end

  defp merge_fragment(expr, args, tag \\ :expr)
  defp merge_fragment([], [], _tag), do: []

  defp merge_fragment([:slot | rest], [arg | rest_args], tag) do
    [{tag, arg} | merge_fragment(rest, rest_args, tag)]
  end

  defp merge_fragment([val | rest], rest_args, tag) do
    [{:raw, val} | merge_fragment(rest, rest_args, tag)]
  end

  defp split_fragment(frag, consumed \\ "")

  defp split_fragment(<<>>, consumed),
    do: [consumed]

  defp split_fragment(<<??, rest::binary>>, consumed),
    do: [consumed, :slot | split_fragment(rest, "")]

  defp split_fragment(<<?\\, ??, rest::binary>>, consumed),
    do: split_fragment(rest, consumed <> <<??>>)

  defp split_fragment(<<first::utf8, rest::binary>>, consumed),
    do: split_fragment(rest, consumed <> <<first::utf8>>)

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{extra: %{original: args}}, opts) do
      container_doc("fragment(", args, ")", opts, &to_doc/2, separator: ",")
    end

    def inspect(%{arguments: args}, opts) do
      container_doc("fragment(", args, ")", opts, &to_doc/2, separator: ",")
    end
  end
end
