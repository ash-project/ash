# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Verifiers.VerifyDelivery do
  @moduledoc """
  Verifies that each signal's delivery implements `Ash.Signals.Delivery`, and lets the delivery
  verify the signal.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.{Entity, Verifier}
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Verifier.get_entities([:signals])
    |> Enum.each(fn %{delivery: {delivery, opts}} = signal ->
      if !(Code.ensure_loaded?(delivery) && function_exported?(delivery, :deliver, 4)) do
        raise_error(
          module,
          signal,
          "`#{inspect(delivery)}` does not implement `Ash.Signals.Delivery`."
        )
      end

      if function_exported?(delivery, :verify_signal, 3) do
        case delivery.verify_signal(signal, opts, dsl_state) do
          :ok -> :ok
          {:error, message} -> raise_error(module, signal, message)
        end
      end
    end)

    :ok
  end

  defp raise_error(module, signal, message) do
    raise DslError,
      module: module,
      path: [:signals, :signal, signal.name, :delivery],
      location: Entity.anno(signal),
      message: message
  end
end
