defmodule Ash.Policy.Authorizer.Verifiers.VerifySatSolverImplementation do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(_dsl) do
    Ash.SatSolver.Implementation.check!()
    :ok
  end
end
