# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid.Argument do
  @moduledoc false
  import Reactor.Mermaid.Utils
  alias Reactor.{Argument, Mermaid.Node}
  require Argument
  @behaviour Reactor.Mermaid

  @doc false
  def to_mermaid(argument, options) when Argument.is_from_value(argument) do
    target_id =
      options
      |> Keyword.fetch!(:target_id)

    source_id =
      argument.source.value
      |> mermaid_id(:value)

    {:ok,
     %Node{
       pre: [
         source_id,
         "{{\"`",
         md_escape(inspect(argument.source.value, pretty: true)),
         "`\"}}\n",
         do_argument_link(source_id, target_id, argument, options)
       ]
     }}
  end

  def to_mermaid(argument, options) when Argument.is_from_input(argument) do
    target_id =
      options
      |> Keyword.fetch!(:target_id)

    source_id =
      {options[:reactor_id], argument.source.name}
      |> mermaid_id(:input)

    content =
      source_id
      |> do_argument_link(target_id, argument, options)

    {:ok, %Node{pre: content}}
  end

  def to_mermaid(argument, options) do
    target_id =
      options
      |> Keyword.fetch!(:target_id)

    source_id =
      {options[:reactor_id], argument.source.name}
      |> mermaid_id(:step)

    content =
      source_id
      |> do_argument_link(target_id, argument, options)

    {:ok, %Node{pre: content}}
  end

  defp do_argument_link(source_id, target_id, argument, options) do
    if options[:describe?] && is_binary(argument.description) do
      "#{source_id} -->|#{name(argument.name)} -- #{argument.description}|#{target_id}\n"
    else
      "#{source_id} -->|#{name(argument.name)}|#{target_id}\n"
    end
  end
end
