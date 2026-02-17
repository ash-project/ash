# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid.Reactor do
  @moduledoc false
  alias Reactor.Mermaid.{Node, Step}
  import Reactor.Utils
  import Reactor.Mermaid.Utils
  @behaviour Reactor.Mermaid

  @doc false
  @impl true
  def to_mermaid(reactor, options) when is_struct(reactor, Reactor) do
    id = mermaid_id(reactor.id, :reactor)

    with {:ok, reactor} <- Reactor.Planner.plan(reactor),
         {:ok, inputs} <- generate_inputs(reactor, options),
         {:ok, steps} <- generate_steps(reactor, options) do
      return_step_id = mermaid_id({reactor.id, reactor.return}, :step)
      return_id = mermaid_id(reactor.id, :return)

      {:ok,
       %Node{
         id: id,
         pre: [
           "subgraph ",
           id,
           "[\"",
           name(reactor.id),
           "\"]"
         ],
         children: [
           "\n",
           "direction ",
           direction(options[:direction]),
           "\n",
           inputs,
           steps,
           return_id,
           "{\"Return\"}\n",
           return_step_id,
           "==>",
           return_id
         ],
         post: [
           "\n",
           "end\n"
         ]
       }}
    end
  end

  defp generate_inputs(reactor, options) do
    reactor.inputs
    |> map_while_ok(&generate_input(&1, reactor, options))
  end

  defp generate_input(input, reactor, options) do
    id = mermaid_id({reactor.id, input.name}, :input)

    content =
      if options[:describe?] do
        [
          id,
          ">\"`",
          "**Input ",
          to_string(input.name),
          "**",
          "\n",
          md_escape(input.description),
          "`\"]",
          "\n"
        ]
      else
        [id, ">\"Input ", to_string(input.name), "\"]\n"]
      end

    {:ok, %Node{id: id, pre: content}}
  end

  defp generate_steps(reactor, options) do
    options = Keyword.put(options, :reactor_id, reactor.id)

    reactor.plan
    |> Graph.vertices()
    |> map_while_ok(&Step.to_mermaid(&1, options))
  end
end
