# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid.Step do
  @moduledoc false
  import Reactor.Utils
  import Reactor.Mermaid.Utils
  alias Reactor.Mermaid.{Argument, Node}
  @behaviour Reactor.Mermaid

  @doc false
  @impl true
  def to_mermaid(step, options) do
    with {:ok, step_node} <- render_step(step, options),
         {:ok, arguments} <-
           generate_arguments(step.arguments, Keyword.put(options, :target_id, step_node.id)) do
      {:ok, %{step_node | pre: [arguments, step_node.pre]}}
    end
  end

  defp generate_arguments(arguments, options) do
    arguments
    |> map_while_ok(&Argument.to_mermaid(&1, options))
  end

  defp render_step(step, options) do
    module = impl_for(step)

    if Spark.implements_behaviour?(module, Reactor.Mermaid) do
      module.to_mermaid(step, options)
    else
      default_describe_step(step, module, options)
    end
  end

  @doc false
  def default_describe_step(step, module, options) do
    id = mermaid_id({options[:reactor_id], step.name}, :step)

    content =
      if options[:describe?] do
        [
          id,
          "[\"`**",
          md_escape(name(step.name)),
          " \\(",
          inspect(module),
          "\\)**\n",
          md_escape(step.description),
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

  defp impl_for(%{impl: {module, _}}) when is_atom(module), do: module
  defp impl_for(%{impl: module}) when is_atom(module), do: module
end
