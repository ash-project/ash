# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyCalculations do
  @moduledoc """
  Validates calculation configurations.

  Currently checks:
  - Warns when the `load` option is used with expression calculations
  """
  use Spark.Dsl.Verifier

  alias Spark.Error.DslError

  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    warnings =
      dsl
      |> Ash.Resource.Info.calculations()
      |> Enum.flat_map(fn calculation ->
        verify_load_with_expression(calculation, module)
      end)

    {:warn, Enum.map(warnings, &Exception.message/1)}
  end

  defp verify_load_with_expression(calculation, module) do
    is_expr_calc =
      case calculation.calculation do
        {Ash.Resource.Calculation.Expression, _} -> true
        _ -> false
      end

    has_load = calculation.load != [] && calculation.load != nil

    if is_expr_calc && has_load do
      [
        DslError.exception(
          module: module,
          message: """
          The `load` option is used on expression calculation `#{calculation.name}`, \
          but `load` only works with Elixir calculations (module-based or function-based).

          Expression calculations automatically determine their dependencies. \
          If you need to use the `load` option, convert this to an Elixir calculation module.
          """,
          path: [:calculations, calculation.name]
        )
      ]
    else
      []
    end
  end
end
