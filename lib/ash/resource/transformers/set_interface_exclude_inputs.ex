# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.SetInterfaceExcludeInputs do
  @moduledoc false
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    dsl_state
    |> set_action_interfaces()
    |> set_calculation_interfaces()
    |> then(&{:ok, &1})
  end

  defp set_action_interfaces(dsl_state) do
    dsl_state
    |> Ash.Resource.Info.interfaces()
    |> Enum.reduce(dsl_state, fn interface, dsl_state ->
      exclude_inputs =
        interface.custom_inputs
        |> Enum.filter(&(&1.transform && &1.transform.to && &1.name in interface.args))
        |> Enum.map(& &1.transform.to)
        |> Enum.concat(interface.exclude_inputs)
        |> Enum.uniq()

      new_interface = %{
        interface
        | exclude_inputs: exclude_inputs
      }

      Transformer.replace_entity(
        dsl_state,
        [:code_interface],
        new_interface,
        fn existing_interface ->
          existing_interface == interface
        end
      )
    end)
  end

  defp set_calculation_interfaces(dsl_state) do
    dsl_state
    |> Ash.Resource.Info.calculation_interfaces()
    |> Enum.reduce(dsl_state, fn interface, dsl_state ->
      exclude_inputs =
        interface.custom_inputs
        |> Enum.filter(&(&1.transform && &1.transform.to && &1.name in interface.args))
        |> Enum.map(& &1.transform.to)
        |> Enum.concat(interface.exclude_inputs)
        |> Enum.uniq()

      new_interface = %{
        interface
        | exclude_inputs: exclude_inputs
      }

      Transformer.replace_entity(
        dsl_state,
        [:code_interface],
        new_interface,
        fn existing_interface ->
          existing_interface == interface
        end
      )
    end)
  end
end
