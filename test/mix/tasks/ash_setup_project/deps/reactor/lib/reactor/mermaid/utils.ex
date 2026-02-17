# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid.Utils do
  @moduledoc """
  Utilities for generating Mermaid.
  """

  @doc "Escape markdown as needed"
  def md_escape(nil), do: ""

  def md_escape(md) do
    md
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "&quot;")
    |> String.replace("`", "&#96;")
    |> String.replace("*", "\\*")
    |> String.replace("_", "\\_")
    |> String.replace("{", "\\{")
    |> String.replace("}", "\\}")
    |> String.replace("[", "\\[")
    |> String.replace("]", "\\]")
    |> String.replace("<", "\\<")
    |> String.replace(">", "\\>")
    |> String.replace("(", "\\(")
    |> String.replace(")", "\\)")
    |> String.replace("#", "\\#")
    |> String.replace("+", "\\+")
    |> String.replace("-", "\\-")
    |> String.replace(".", "\\.")
    |> String.replace("!", "\\!")
    |> String.replace("|", "\\|")
  end

  @doc "Generate a mermaid ID for a term"
  @spec mermaid_id(any, String.Chars.t()) :: String.t()
  def mermaid_id(name, prefix) when is_atom(name) do
    name =
      name
      |> to_string()
      |> deelixirify()

    if Regex.match?(~r/\A[a-zA-Z0-9\._-]+\Z/, name) do
      "#{prefix}_#{name}"
    else
      "#{prefix}_#{:erlang.phash2(name)}"
    end
  end

  def mermaid_id(name, prefix) when is_binary(name) do
    if Regex.match?(~r/\A[a-zA-Z0-9\._-]+\Z/, name) do
      "#{prefix}_#{name}"
    else
      "#{prefix}_#{:erlang.phash2(name)}"
    end
  end

  def mermaid_id(name, prefix), do: "#{prefix}_#{:erlang.phash2(name)}"

  @doc "Generate a name which can be used within a Mermaid node"
  def name(name) when is_binary(name) do
    if String.printable?(name) do
      name
    else
      inspect(name)
    end
  end

  def name(name) when is_atom(name) do
    name
    |> to_string()
    |> deelixirify()
  end

  def name(name), do: inspect(name)

  defp deelixirify(name) do
    if String.starts_with?(name, "Elixir.") do
      String.replace_leading(name, "Elixir.", "")
    else
      name
    end
  end

  @doc "Convert the direction atom into a mermaid direction"
  @spec direction(atom) :: String.t()
  def direction(:top_to_bottom), do: "TB"
  def direction(:bottom_to_top), do: "BT"
  def direction(:left_to_right), do: "LR"
  def direction(:right_to_left), do: "RL"
end
