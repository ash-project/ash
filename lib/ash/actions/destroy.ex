defmodule Ash.Actions.Destroy do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.api(), Ash.record(), Ash.action(), Keyword.t()) ::
          :ok | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, opts) do
    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    authorization_request =
      Request.new(
        resource: resource,
        api: api,
        path: [:data],
        action: action,
        data: [record],
        name: "destroy request"
      )

    destroy_request =
      Request.new(
        resource: resource,
        api: api,
        path: [:destroy],
        action: action,
        authorize?: false,
        data:
          Request.resolve([[:data, :data]], fn _ ->
            case Ash.data_layer(resource).destroy(record) do
              :ok -> {:ok, record}
              {:error, error} -> {:error, error}
            end
          end)
      )

    case Engine.run([authorization_request, destroy_request], api, engine_opts) do
      %{errors: []} ->
        :ok

      {:error, errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}
    end
  end
end
