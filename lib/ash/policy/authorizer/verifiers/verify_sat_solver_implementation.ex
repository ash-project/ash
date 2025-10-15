# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Authorizer.Verifiers.VerifySatSolverImplementation do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(_dsl) do
    Crux.Implementation.check!()
    :ok
  end
end
