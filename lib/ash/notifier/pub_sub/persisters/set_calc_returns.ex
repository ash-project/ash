# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier.PubSub.Persisters.SetCalcReturns do
  @moduledoc """
  Sets `returns` and `constraints` on publications whose `transform` names a calculation.

  Runs after `ResolveAutoTypes` so that `:auto` types have been resolved.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def before?(Ash.Resource.Transformers.CacheCalculations), do: true
  def before?(_), do: false

  def transform(dsl_state) do
    dsl_state =
      dsl_state
      |> Ash.Notifier.PubSub.Info.publications()
      |> Enum.reduce(dsl_state, fn publication, dsl ->
        if is_atom(publication.transform) and not is_nil(publication.transform) and
             is_nil(publication.returns) do
          case Ash.Resource.Info.calculation(dsl, publication.transform) do
            %{type: type, constraints: constraints} when type not in [:auto, nil] ->
              updated = %{publication | returns: type, constraints: constraints}

              Transformer.replace_entity(
                dsl,
                [:pub_sub],
                updated,
                &(&1 == publication)
              )

            _ ->
              dsl
          end
        else
          dsl
        end
      end)

    {:ok, dsl_state}
  end
end
