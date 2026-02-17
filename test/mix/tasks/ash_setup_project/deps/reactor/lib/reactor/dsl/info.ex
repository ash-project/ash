# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Info do
  @moduledoc """
  Introspection for the Reactor DSL.
  """
  use Spark.InfoGenerator, sections: [:reactor], extension: Reactor.Dsl

  alias Reactor.{Builder, Dsl}
  alias Spark.Dsl.Extension
  import Reactor.Utils

  @doc """
  Convert a reactor DSL module into a reactor struct.
  """
  @spec to_struct(module | Reactor.t() | Spark.Dsl.t()) :: {:ok, Reactor.t()} | {:error, any}
  def to_struct(reactor) when is_struct(reactor, Reactor), do: {:ok, reactor}

  def to_struct(module) do
    with {:ok, reactor} <- entities_to_struct(module),
         {:ok, reactor} <- maybe_set_return(module, reactor) do
      add_middleware(module, reactor)
    end
  end

  @doc """
  Raising version of `to_struct/1`.
  """
  @spec to_struct!(module | Reactor.t() | Spark.Dsl.t()) :: Reactor.t() | no_return
  def to_struct!(reactor) do
    case to_struct(reactor) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  defp entities_to_struct(module) when is_atom(module) do
    module
    |> reactor()
    |> reduce_while_ok(Builder.new(module), &Dsl.Build.build/2)
  end

  defp entities_to_struct(dsl_state) when is_map(dsl_state) do
    module = Extension.get_persisted(dsl_state, :module)

    dsl_state
    |> reactor()
    |> reduce_while_ok(Builder.new(module), &Dsl.Build.build/2)
  end

  defp maybe_set_return(module, reactor) do
    case reactor_return(module) do
      {:ok, value} -> {:ok, %{reactor | return: value}}
      :error -> {:ok, reactor}
    end
  end

  defp add_middleware(module, reactor) do
    module
    |> reactor_middlewares()
    |> reduce_while_ok(reactor, &Dsl.Build.build/2)
  end
end
