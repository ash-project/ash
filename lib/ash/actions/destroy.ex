defmodule Ash.Actions.Destroy do
  alias Ash.Engine

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, params) do
    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

    auth_request =
      Ash.Engine.Request.new(
        resource: resource,
        rules: action.rules,
        api: api,
        strict_access: false,
        path: [:data],
        data:
          Ash.Engine.Request.resolve(fn _ ->
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
          bypass_strict_access?: params[:bypass_strict_access?],
          verbose?: params[:verbose?]
        )
      else
        Engine.run([auth_request], api, fetch_only?: true, verbose?: params[:verbose?])
      end

    case result do
      %{errors: errors} when errors == %{} ->
        :ok

      %Ash.Engine{errors: errors} ->
        errors =
          Enum.flat_map(errors, fn {path, errors} ->
            Enum.map(errors, &Map.put(&1, :path, path))
          end)

        {:error, Ash.to_ash_error(errors)}
    end
  end
end
