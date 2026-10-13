# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.SetSignalsOut do
  @moduledoc false
  # Adds the signal modules of `emit_signal` changes and preparations to `signals_out`.
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    case Ash.Signals.Emitter.usages(dsl_state) do
      [] ->
        {:ok, dsl_state}

      usages ->
        signals_out =
          dsl_state
          |> Transformer.get_persisted(:signals_out, [])
          |> Enum.concat(Enum.map(usages, fn {_, _, _, opts} -> opts[:signal_module] end))
          |> Enum.uniq()

        {:ok, Transformer.persist(dsl_state, :signals_out, signals_out)}
    end
  end
end
