defmodule Ash.Actions.Destroy do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          :ok | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, _params) do
    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

    request =
      Request.new(
        resource: resource,
        api: api,
        path: [:data],
        action: action,
        request_id: :change,
        data:
          Request.resolve(fn _ ->
            case Ash.data_layer(resource).destroy(record) do
              :ok -> {:ok, record}
              {:error, error} -> {:error, error}
            end
          end),
        name: "destroy request"
      )

    case Engine.run([request], api) do
      %{errors: []} ->
        :ok

      %Engine{errors: errors} ->
        {:error, Ash.to_ash_error(errors)}
    end
  end
end
