# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Transform.Mermaid do
  @moduledoc """
  Mermaid rendering for transform steps.
  """

  alias Reactor.Mermaid.{Node, Utils}
  import Utils

  @doc false
  def to_mermaid(step, fun, options) do
    id = mermaid_id({options[:reactor_id], step.name}, :step)

    name =
      case step.name do
        {:__reactor__, :transform, :input, input} ->
          "Transform input #{name(input)}"

        {:__reactor__, :transform, argument_name, _step_name} ->
          "Transform argument #{name(argument_name)}"

        other ->
          name(other)
      end

    content =
      if options[:describe?] do
        [
          id,
          "[\"`**",
          name,
          "**\n_",
          md_escape(inspect(fun)),
          "_`\"]\n"
        ]
      else
        [id, "[", name, "]\n"]
      end

    {:ok, %Node{id: id, pre: content}}
  end
end
