# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Switch.Default do
  @moduledoc """
  The `default` DSL entity struct.

  See `d:Reactor.switch.default`.
  """

  defstruct __identifier__: nil, return: nil, steps: [], __spark_metadata__: nil

  alias Reactor.Dsl

  @type t :: %__MODULE__{
          __identifier__: any,
          return: nil | atom,
          steps: [Dsl.Step.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }
end
