defmodule Ash.Reactor.TransactionStep do
  @moduledoc """
  The Reactor step which is used to wrap other steps in an Ash data layer
  transaction.
  """
  use Reactor.Step

  def run(arguments, context, options) do
    sub_reactor = Keyword.fetch!(options, :sub_reactor)
    resources = Keyword.fetch!(options, :resources)
    timeout = Keyword.fetch!(options, :timeout)

    Ash.DataLayer.transaction(
      resources,
      fn ->
        case Reactor.run(sub_reactor, arguments, context, async?: false) do
          {:ok, result} ->
            result

          {:error, [reason]} ->
            Ash.DataLayer.rollback(resources, reason)

          {:error, reasons} ->
            Ash.DataLayer.rollback(resources, reasons)
        end
      end,
      timeout
    )
  end
end
