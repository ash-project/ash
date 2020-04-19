defmodule Ash.Actions.Destroy do
  alias Ash.Engine

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, params) do
    transaction_result =
      Ash.DataLayer.transact(resource, fn ->
        do_authorized(api, params, action, record)
      end)

    case transaction_result do
      {:ok, value} -> value
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorized(api, params, action, %resource{} = record) do
    auth_request =
      Ash.Engine.Request.new(
        resource: resource,
        rules: action.rules,
        api: api,
        strict_access: false,
        path: [:data],
        data:
          Ash.Engine.Request.UnresolvedField.data([], fn _request, _ ->
            case Ash.data_layer(resource).destroy(record) do
              :ok -> {:ok, record}
              {:error, error} -> {:error, error}
            end
          end),
        name: "destroy request",
        resolve_when_fetch_only?: true
      )

    if params[:authorization] do
      Engine.run(
        [auth_request],
        api,
        user: params[:authorization][:user],
        log_final_report?: params[:authorization][:log_final_report?]
      )
    else
      Engine.run([auth_request], api, fetch_only?: true)
    end
  end
end
