# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Map.Mermaid do
  @moduledoc """
  Mermaid rendering for `map` steps.
  """

  alias Reactor.{Argument, Builder, Mermaid, Template}
  import Reactor.Utils
  import Mermaid.Utils
  require Argument

  @doc false
  def to_mermaid(%{impl: {Reactor.Step.Map, opts}} = step, options) do
    steps = Keyword.get(opts, :steps, [])

    with {:ok, reactor} <- build_nested_reactor(step.arguments, step.name, steps, opts[:return]),
         {:ok, sub_graph} <- Mermaid.Reactor.to_mermaid(reactor, options),
         {:ok, node} <- describe_step(step, options) do
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

  defp build_nested_reactor(arguments, name, steps, return) do
    reactor = Builder.new({__MODULE__, name})

    with {:ok, reactor} <- build_inputs(reactor, arguments),
         {:ok, reactor} <- build_steps(reactor, steps) do
      {:ok, %{reactor | return: return}}
    end
  end

  defp build_inputs(reactor, arguments) do
    arguments
    |> map_while_ok(&Argument.Build.build/1)
    |> and_then(&{:ok, List.flatten(&1)})
    |> and_then(fn arguments ->
      reduce_while_ok(arguments, reactor, &Builder.add_input(&2, &1.name))
    end)
  end

  defp build_steps(reactor, steps) do
    steps =
      Enum.map(steps, fn step ->
        arguments =
          Enum.map(step.arguments, fn
            argument when Argument.is_from_element(argument) ->
              %{argument | source: %Template.Input{name: :source}}

            argument ->
              argument
          end)

        %{step | arguments: arguments}
      end)

    {:ok, %{reactor | steps: Enum.concat(steps, reactor.steps)}}
  end

  defp describe_step(%{impl: {module, _opts}} = step, options) do
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

    {:ok, %Mermaid.Node{id: id, pre: content}}
  end
end
