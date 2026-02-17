# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Mermaid do
  @moduledoc """
  Converts Reactors and their related entities into a Mermaid diagram.

  See [Mermaid](https://mermaid.js.org/) for more information.
  """

  @options Spark.Options.new!(
             expand?: [
               type: :boolean,
               required: false,
               default: false,
               doc: "Whether or not to expand composed Reactors"
             ],
             describe?: [
               type: :boolean,
               required: false,
               default: false,
               doc: "Whether or not to include descriptions, if available"
             ],
             direction: [
               type: {:in, [:top_to_bottom, :bottom_to_top, :right_to_left, :left_to_right]},
               required: false,
               default: :left_to_right,
               doc: "The direction to render the flowchart"
             ],
             indent: [
               type: :non_neg_integer,
               required: false,
               default: 0,
               doc: "How much to indent the resulting mermaid"
             ],
             output: [
               type: {:in, [:iodata, :binary]},
               required: false,
               default: :iodata,
               doc: "Specify the output format. `iodata` is more performant"
             ]
           )

  @type options :: Keyword.t()

  @callback to_mermaid(module | struct, options) :: {:ok, __MODULE__.Node.t()} | {:error, any}

  @doc """
  Convert the Reactor into Mermaid.

  ## Options

  #{Spark.Options.docs(@options)}
  """
  @spec to_mermaid(module | Reactor.t(), options) :: {:ok, iodata()} | {:error, any}
  def to_mermaid(reactor, options \\ []) do
    with {:ok, options} <- Spark.Options.validate(options, @options) do
      do_to_mermaid(reactor, options)
    end
  end

  @doc """
  Convert the Reactor into Mermaid

  Raising version of `to_mermaid/2`
  """
  @spec to_mermaid!(module | Reactor.t(), options) :: iodata() | no_return()
  def to_mermaid!(reactor, options \\ []) do
    case to_mermaid(reactor, options) do
      {:ok, iodata} -> iodata
      {:error, reason} when is_exception(reason) -> raise reason
      {:error, reason} -> raise RuntimeError, reason
    end
  end

  defp do_to_mermaid(reactor, options) when is_atom(reactor) do
    if Code.ensure_loaded?(reactor) && function_exported?(reactor, :spark_is, 0) &&
         reactor.spark_is() == Reactor do
      do_to_mermaid(reactor.reactor(), options)
    else
      {:error, ArgumentError.exception(message: "`reactor` argument is not a Reactor")}
    end
  end

  defp do_to_mermaid(reactor, options) when is_struct(reactor, Reactor) do
    with {:ok, reactor} <- Reactor.Planner.plan(reactor),
         {:ok, sub_graph} <- __MODULE__.Reactor.to_mermaid(reactor, options),
         {:ok, root} <- root_node(sub_graph, options) do
      iodata = __MODULE__.Node.render(root, options[:indent] || 0)

      if options[:output] == :binary do
        string =
          iodata
          |> IO.iodata_to_binary()
          |> String.trim_trailing(" ")

        {:ok, string}
      else
        {:ok, iodata}
      end
    end
  end

  defp root_node(sub_graph, options) do
    {:ok,
     %__MODULE__.Node{
       id: "root",
       pre: [
         "flowchart ",
         __MODULE__.Utils.direction(options[:direction])
       ],
       children: [
         "\n",
         "start{\"Start\"}\n",
         "start==>",
         sub_graph.id,
         "\n",
         sub_graph
       ]
     }}
  end
end
