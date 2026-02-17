# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Switch.Mermaid do
  @moduledoc """
  Mermaid rendering for `switch` steps.
  """

  import Reactor.Utils
  alias Reactor.Mermaid.{Node, Step, Utils}
  import Utils

  @doc false
  def to_mermaid(%{impl: {_module, opts}} = step, options) do
    branches =
      opts
      |> Keyword.get(:matches, [])
      |> Enum.concat([{:default, opts[:default]}])
      |> Enum.reject(fn {_, steps} -> is_nil(steps) || steps == [] end)

    with {:ok, node} <- describe_step(step, options),
         {:ok, branches} <- describe_branches(step, branches, options) do
      decision_id = "#{node.id}_decision"

      decision = [
        decision_id,
        "@{shape: diamond, label: \"Decision for ",
        name(step.name),
        "\"}\n",
        node.id,
        "-->",
        decision_id,
        "\n"
      ]

      links =
        Enum.map(branches, fn branch ->
          [decision_id, "-->", branch.id, "\n"]
        end)

      {:ok, %{node | post: [node.post, decision, links, branches]}}
    end
  end

  defp describe_step(%{impl: {module, _opts}} = step, options) do
    id = mermaid_id({options[:reactor_id], step.name}, :step)

    content =
      if options[:describe?] do
        [
          id,
          "[\"`**",
          name(step.name),
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

  defp describe_branches(step, branches, options) do
    map_while_ok(branches, &describe_branch(step, &1, options))
  end

  defp describe_branch(step, {:default, steps}, options) do
    id = mermaid_id({options[:reactor_id], step.name, :default}, :step)

    with {:ok, steps} <- generate_steps(steps, options) do
      {:ok,
       %Node{
         id: id,
         pre: [
           "subgraph ",
           id,
           "[\"default branch of ",
           name(step.name),
           "\"]"
         ],
         children: [
           "\n",
           steps,
           "direction ",
           direction(options[:direction])
         ],
         post: [
           "\n",
           "end\n"
         ]
       }}
    end
  end

  defp describe_branch(step, {predicate, steps}, options) do
    id = mermaid_id({options[:reactor_id], step.name, predicate}, :step)

    with {:ok, steps} <- generate_steps(steps, options) do
      content =
        if options[:describe?] do
          [
            "subgraph ",
            id,
            "[\"`match branch of ",
            name(step.name),
            "\n_",
            md_escape(inspect(predicate)),
            "_",
            "`\"]"
          ]
        else
          [
            "subgraph ",
            id,
            "[\"match branch of ",
            name(step.name),
            "\"]"
          ]
        end

      {:ok,
       %Node{
         id: id,
         pre: content,
         children: [
           "\n",
           steps,
           "direction ",
           direction(options[:direction])
         ],
         post: [
           "\n",
           "end\n"
         ]
       }}
    end
  end

  defp generate_steps(steps, options) do
    map_while_ok(steps, &Step.to_mermaid(&1, options))
  end
end
