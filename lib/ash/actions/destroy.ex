defmodule Ash.Actions.Destroy do
  alias Ash.Authorization.Authorizer

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(_api, %resource{} = record, action, params) do
    if Keyword.get(params, :side_load, []) in [[], nil] do
      user = Keyword.get(params, :user)

      case do_authorize(params, action, user, record) do
        :authorized ->
          case Ash.data_layer(resource).destroy(record) do
            :ok -> {:ok, record}
            {:error, error} -> {:error, error}
          end

        _ ->
          {:error, :forbidden}
      end
    else
      {:error, "Cannot side load on update currently"}
    end
  end

  defp do_authorize(params, action, user, %resource{} = record) do
    if Keyword.get(params, :authorize?, false) do
      auth_request =
        Ash.Authorization.Request.new(
          resource: resource,
          authorization_steps: action.authorization_steps,
          destroy: record
        )

      Authorizer.authorize(user, [auth_request])
    else
      :authorized
    end
  end
end
