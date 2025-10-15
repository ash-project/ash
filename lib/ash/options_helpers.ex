# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.OptionsHelpers do
  @moduledoc """
  Helpers for working with nimble options
  """

  @doc """
  Specifies Ash types that can be allowed when declaring a type for an
  attribute, calculation, aggregate, etc.
  """
  @spec ash_type() :: {:spark_type, Ash.Type, :builtins, [{String.t(), String.t(), String.t()}]}
  def ash_type do
    {:spark_type, Ash.Type, :builtins,
     [{"{:array, inner_type}", "list", "{:array, ${1:inner_type}}"}]}
  end

  @doc """
  Used to configure Spark to know that the type referenced is an Ash.Resource
  also using Spark.
  """
  @spec ash_resource() :: {:spark, Ash.Resource}
  def ash_resource do
    {:spark, Ash.Resource}
  end

  @doc """
  Used for marking an option as hidden so it doesn't show up as an option
  with Spark.
  """
  @spec hide_all_except(Keyword.t(), [atom()]) :: Keyword.t()
  def hide_all_except(options, keys) do
    Enum.map(options, fn {key, config} ->
      if key in keys do
        {key, config}
      else
        {key, Keyword.put(config, :hide, true)}
      end
    end)
  end

  @doc """
  Defines the calculation type used in DSL schema definitions, allowing either
  a calculation module/function or an `expr(...)` expression.
  """
  @spec calculation_type() :: Spark.Options.type()
  def calculation_type do
    {:or,
     [
       {:spark_function_behaviour, Ash.Resource.Calculation,
        {Ash.Resource.Calculation.Function, 2}},
       {:custom, Ash.Resource.Calculation, :expr_calc, []}
     ]}
  end
end
