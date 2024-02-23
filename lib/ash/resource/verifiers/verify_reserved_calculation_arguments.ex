defmodule Ash.Resource.Verifiers.VerifyReservedCalculationArguments do
  @moduledoc "Verifies that standard context keys are not used as calculation arguments"
  use Spark.Dsl.Verifier

  @reserved_calculation_argument_names ~w(as)a

  def verify(dsl) do
    dsl
    |> Ash.Resource.Info.calculations()
    |> Enum.each(fn calculation ->
      Enum.each(calculation.arguments, fn argument ->
        if argument.name in @reserved_calculation_argument_names do
          raise Spark.Error.DslError,
            module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
            message:
              "Argument #{argument.name} is reserved and cannot be used in a calculation argument.",
            path: [:calculations, calculation.name, :argument, argument.name]
        end
      end)
    end)
  end
end
