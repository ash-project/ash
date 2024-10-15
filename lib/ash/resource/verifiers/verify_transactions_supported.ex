defmodule Ash.Resource.Verifiers.VerifyTransactionsSupported do
  @moduledoc """
  Validates that all actions have `transaction? false` when the resource's data layer says they're not supported
  """
  use Spark.Dsl.Verifier
  alias Ash.Resource.Info
  alias Spark.{Dsl.Verifier, Error.DslError}

  @doc false
  @impl true
  def verify(dsl) do
    resource = Verifier.get_persisted(dsl, :module)
    data_layer = Info.data_layer(resource)

    if function_exported?(data_layer, :can?, 2) do
      cond do
        data_layer.can?(resource, :transact) ->
          :ok

        data_layer.can?(resource, :inhibit_transaction_validation) ->
          :ok

        true ->
          verify_action_transactions(dsl, data_layer, resource)
      end
    else
      :ok
    end
  end

  defp verify_action_transactions(dsl, data_layer, resource) do
    dsl
    |> Info.actions()
    |> Enum.filter(&(&1.transaction? == true))
    |> case do
      [] ->
        :ok

      [action] ->
        error =
          DslError.exception(
            path: [:actions, action.name],
            module: resource,
            message: """
            The data layer `#{inspect(data_layer)}` does not support transactions.

            Please set `transaction? false` for the `#{inspect(action.name)}` #{action.type} action.
            """
          )

        {:error, error}

      actions ->
        actions =
          actions
          |> Enum.map_join("\n", &"  - #{&1.type} `#{inspect(&1.name)}`")

        error =
          DslError.exception(
            path: [:actions],
            module: resource,
            message: """
            The data layer `#{inspect(data_layer)}` does not support transactions.
              
            Please set `transaction? false` for the following actions:

            #{actions}
            """
          )

        {:error, error}
    end
  end
end
