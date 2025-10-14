# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.CalculationInterface do
  @moduledoc """
  Represents a function that evaluates a calculation in a resource's code interface
  """
  defstruct [
    :name,
    :calculation,
    args: [],
    exclude_inputs: [],
    custom_inputs: [],
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{__spark_metadata__: Spark.Dsl.Entity.spark_meta()}

  @schema [
    name: [
      type: :atom,
      doc: "The name of the function that will be defined",
      required: true
    ],
    calculation: [
      type: :atom,
      doc:
        "The name of the calculation that will be evaluated. Defaults to the same name as the function."
    ],
    exclude_inputs: [
      type: {:list, :atom},
      default: [],
      doc: "A list of calculation inputs to not accept in the defined interface"
    ],
    args: [
      type: :any,
      default: [],
      doc: """
      Supply field or argument values referenced by the calculation, in the form of :name, `{:arg, :name}` and/or `{:ref, :name}`. See the [code interface guide](/documentation/topics/resources/code-interfaces.md) for more.
      """
    ]
  ]

  def schema, do: @schema

  def transform(%{name: name, calculation: nil} = interface) do
    {:ok, %{interface | calculation: name}}
  end

  def transform(interface), do: {:ok, interface}
end
