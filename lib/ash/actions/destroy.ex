defmodule Ash.Actions.Destroy do
  alias Ash.Engine

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(_api, %resource{} = record, action, params) do
    raise "what"
    # if Keyword.get(params, :side_load, []) in [[], nil] do
    #   user = Keyword.get(params, :user)

    #   transaction_result =
    #     Ash.DataLayer.transact(resource, fn ->
    #       do_authorized(params, action, user, record)
    #     end)

    #   case transaction_result do
    #     {:ok, value} -> value
    #     {:error, error} -> {:error, error}
    #   end
    # else
    #   {:error, "Cannot side load on update currently"}
    # end
  end

  # defp do_authorized(params, action, user, %resource{} = record) do
  #   if params[:authorization] do
  #     auth_request =
  #       Ash.Engine.Request.new(
  #         resource: resource,
  #         rules: action.rules,
  #         data:
  #           Ash.Engine.Request.UnresolvedField.data([], fn _request, _ ->
  #             case Ash.data_layer(resource).destroy(record) do
  #               :ok -> {:ok, record}
  #               {:error, error} -> {:error, error}
  #             end
  #           end),
  #         name: "destroy request",
  #         resolve_when_fetch_only?: true
  #       )

  #     Engine.run(user, [auth_request])
  #   else
  #     :authorized
  #   end
  # end
end
