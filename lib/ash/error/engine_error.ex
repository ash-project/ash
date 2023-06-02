defmodule Ash.Error.EngineError do
  @moduledoc "Used when the Ash engine has an internal error"
  use Ash.Error.Exception

  def_ash_error([:state, :message], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "engine_error"

    def message(%{state: state, message: message}) do
      transaction_info =
        if state.transaction_id do
          "Transaction: #{inspect(state.transaction_id)}"
        end

      """
      #{message}
      #{transaction_info}
      #{Ash.Engine.long_breakdown(state)}
      """
    end
  end
end
