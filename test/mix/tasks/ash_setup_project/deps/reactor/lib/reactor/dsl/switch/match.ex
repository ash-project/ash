# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Switch.Match do
  @moduledoc """
  The `matches?` DSL entity struct.

  See `d:Reactor.switch.matches?`.
  """

  defstruct __identifier__: nil,
            allow_async?: true,
            predicate: nil,
            return: nil,
            steps: [],
            __spark_metadata__: nil

  alias Reactor.Dsl.Step

  @type t :: %__MODULE__{
          __identifier__: any,
          allow_async?: boolean,
          predicate: (any -> any),
          return: nil | atom,
          steps: [Step.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }
end
