# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.AnonFn.Mermaid do
  @moduledoc """
  Mermaid rendering for `AnonFn` steps.
  """

  alias Reactor.Mermaid.{Node, Utils}
  import Utils

  @doc false
  def to_mermaid(%{impl: {module, opts}} = step, options) do
    id = mermaid_id({options[:reactor_id], step.name}, :step)

    content =
      if options[:describe?] do
        functions =
          opts
          |> Keyword.take([:run, :compensate, :undo])
          |> Enum.sort_by(&elem(&1, 0))
          |> Enum.reject(&is_nil(elem(&1, 1)))
          |> Enum.map(fn {name, fun} ->
            [
              "- #{name}: _",
              md_escape(inspect(fun)),
              "_"
            ]
          end)
          |> Enum.intersperse(["\n"])

        [
          id,
          "[\"`**",
          name(step.name),
          " \\(",
          inspect(module),
          "\\)**\n",
          functions,
          if(step.description, do: ["\n", md_escape(step.description)], else: []),
          "`\"]\n"
        ]
      else
        [id, "[\"", name(step.name), "(", inspect(module), ")\"]\n"]
      end

    {:ok, %Node{id: id, pre: content}}
  end
end
