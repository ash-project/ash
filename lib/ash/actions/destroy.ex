defmodule Ash.Actions.Destroy do
  alias Ash.Engine

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, params) do
    auth_request =
      Ash.Engine.Request.new(
        resource: resource,
        rules: action.rules,
        api: api,
        strict_access: false,
        path: [:data],
        data:
          Ash.Engine.Request.UnresolvedField.data([], fn _ ->
            case Ash.data_layer(resource).destroy(record) do
              :ok -> {:ok, record}
              {:error, error} -> {:error, error}
            end
          end),
        name: "destroy request",
        resolve_when_fetch_only?: true
      )

    result =
      if params[:authorization] do
        Engine.run(
          [auth_request],
          api,
          user: params[:authorization][:user],
          bypass_strict_access?: params[:bypass_strict_access?]
        )
      else
        Engine.run([auth_request], api, fetch_only?: true)
      end

    case result do
      %{errors: errors} when errors == %{} -> :ok
      %{errors: errors} -> {:error, errors}
    end
  end
end
