# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Domain.Transformers.SetInterfaceExcludeInputs do
  @moduledoc false
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    dsl_state
    |> Ash.Domain.Info.resource_references()
    |> Enum.reduce(dsl_state, fn reference, dsl_state ->
      new_reference =
        Map.update!(reference, :definitions, fn definitions ->
          Enum.map(definitions, fn interface ->
            exclude_inputs =
              interface.custom_inputs
              |> Enum.filter(&(&1.transform && &1.transform.to && &1.name in interface.args))
              |> Enum.map(& &1.transform.to)
              |> Enum.concat(interface.exclude_inputs)
              |> Enum.uniq()

            %{interface | exclude_inputs: exclude_inputs}
          end)
        end)

      Transformer.replace_entity(dsl_state, [:resources], new_reference, &(&1 == reference))
    end)
    |> then(&{:ok, &1})
  end
end
