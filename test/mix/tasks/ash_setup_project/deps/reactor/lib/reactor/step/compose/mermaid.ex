# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Compose.Mermaid do
  @moduledoc """
  Mermaid rendering for `compose` steps.
  """

  alias Reactor.Mermaid.{Node, Reactor, Utils}
  import Utils

  @doc false
  def to_mermaid(%{impl: {module, opts}} = step, options) do
    reactor =
      case Keyword.fetch!(opts, :reactor) do
        reactor when is_struct(reactor) -> reactor
        reactor when is_atom(reactor) -> reactor.reactor()
      end

    reactor = %{reactor | id: {reactor.id, step.name}}

    with {:ok, sub_graph} <- Reactor.to_mermaid(reactor, options),
         {:ok, node} <- describe_step(step, module, options) do
      inner_return_id = mermaid_id(reactor.id, :return)

      links =
        reactor.inputs
        |> Enum.map(fn input ->
          [node.id, "-->", mermaid_id({reactor.id, input.name}, :input), "\n"]
        end)
        |> Enum.concat([inner_return_id, "-->", node.id, "\n"])

      {:ok, %{node | post: [sub_graph, node.post, links]}}
    end
  end

  defp describe_step(step, module, options) do
    id = mermaid_id({options[:reactor_id], step.name}, :step)

    content =
      if options[:describe?] do
        [
          id,
          "[\"`**",
          md_escape(name(step.name)),
          " \\(",
          inspect(module),
          "\\)**",
          if(step.description, do: ["\n", md_escape(step.description)], else: []),
          "`\"]\n"
        ]
      else
        [
          id,
          "[\"",
          name(step.name),
          "(",
          inspect(module),
          ")\"]\n"
        ]
      end

    {:ok, %Node{id: id, pre: content}}
  end
end
