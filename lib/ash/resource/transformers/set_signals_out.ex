# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.SetSignalsOut do
  @moduledoc false
  # Persists the signal modules that the resource's actions emit to, as `signals_out`.
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    signals_out =
      for action <- Transformer.get_entities(dsl_state, [:actions]),
          %{signal_module: signal_module} <- Map.get(action, :emits, []),
          uniq: true,
          do: signal_module

    {:ok, Transformer.persist(dsl_state, :signals_out, signals_out)}
  end
end
