# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defprotocol Reactor.Dsl.Build do
  @moduledoc """
  A protocol which DSL entities must implement.
  """

  @doc """
  Build an entity into a Reactor.

  This function is called during conversion of a DSL module into a Reactor
  struct.  This allows extensions to specify the behaviour of how they want
  to alter the structure of the Reactor.
  """
  @spec build(t, Reactor.t()) :: {:ok, Reactor.t()} | {:error, any}
  def build(entity, reactor)

  @doc """
  Perform any after-compilation verification that is needed to make the entity
  work.

  See `Spark.Dsl.Verifier` for more information.
  """
  @spec verify(t, Spark.Dsl.t()) :: :ok | {:error, any}
  def verify(entity, dsl_state)
end
