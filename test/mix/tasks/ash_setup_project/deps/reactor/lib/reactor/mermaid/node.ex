# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid.Node do
  @moduledoc """
  A node in the mermaid graph
  """
  defstruct children: [], id: nil, post: [], pre: []

  @type node_data :: node_list | binary | byte | t
  @type node_list :: [node_data] | node_data

  @type t :: %__MODULE__{
          children: node_list,
          id: nil | String.t(),
          post: node_list,
          pre: node_list
        }

  @indent "    "

  @doc """
  Render the node at the provided indent level.
  """
  @spec render(t, non_neg_integer()) :: iodata
  def render(node, indent) do
    indent =
      [@indent]
      |> Stream.cycle()
      |> Enum.take(indent)

    do_render([node], [], indent)
  end

  defguardp is_byte(byte) when is_integer(byte) and byte >= 0 and byte <= 255

  defp do_render([], result, _indent), do: result
  defp do_render([[] | tail], result, indent), do: do_render(tail, result, indent)

  defp do_render([head | tail], result, indent) when is_list(head) do
    head = do_render(head, [], indent)
    do_render(tail, [result, head], indent)
  end

  defp do_render([head | tail], result, indent) when is_struct(head, __MODULE__) do
    pre = do_render(head.pre, [], indent)
    children = do_render(head.children, [], [indent, @indent])
    post = do_render(head.post, [], indent)
    do_render(tail, [result, pre, children, post], indent)
  end

  defp do_render([head | tail], result, indent) when is_binary(head),
    do: do_render(tail, [result, indent(head, indent)], indent)

  defp do_render([head | tail], result, indent)
       when is_integer(head) and head >= 0 and head <= 255,
       do: do_render(tail, [result, indent(head, indent)], indent)

  defp do_render(bin, result, indent) when is_binary(bin),
    do: do_render([], [result, indent(bin, indent)], indent)

  defp do_render(int, result, indent) when is_byte(int),
    do: do_render([], [result, indent(int, indent)], indent)

  defp indent("", _indent), do: ""

  defp indent(input, []), do: input

  defp indent(bin, indent) when is_binary(bin) do
    bin
    |> String.split("\n")
    |> Enum.intersperse(["\n", indent])
  end

  defp indent(?\n, indent) do
    [?\n, indent]
  end

  defp indent(int, _indent) when is_byte(int), do: IO.chardata_to_string([int])
end
