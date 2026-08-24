# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.SetActionTransactions do
  @moduledoc """
  Disables transactions on mutation actions when the data layer cannot transact.

  Create, update and destroy actions default to `transaction? true`, which the
  runtime then ignores for a data layer that does not support transactions. This
  makes the action say what will actually happen.
  """

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  @mutations [:create, :update, :destroy]

  @doc false
  @spec transform(Spark.Dsl.t()) :: {:ok, Spark.Dsl.t()}
  def transform(dsl_state) do
    if Ash.DataLayer.can?(:transact, dsl_state) do
      {:ok, dsl_state}
    else
      transacting_actions =
        dsl_state
        |> Transformer.get_entities([:actions])
        |> Enum.filter(&(&1.type in @mutations && &1.transaction?))

      {:ok,
       Enum.reduce(transacting_actions, dsl_state, fn action, dsl_state ->
         Transformer.replace_entity(
           dsl_state,
           [:actions],
           %{action | transaction?: false},
           &(&1.name == action.name)
         )
       end)}
    end
  end
end
