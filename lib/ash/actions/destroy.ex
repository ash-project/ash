defmodule Ash.Actions.Destroy do
  alias Ash.Engine

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, params) do
    if params[:authorization][:bypass_strict_access?] do
      # This one is a bit weird. Essentially, we can't *fetch* the data without *deleting*
      # it, as there is only one data resolution step. Specifically to support deletes,
      # we may need to add a final step, like a "commit" or "operation" that happens
      # after data is fetched and authorization is complete. That would let us support
      # bypassing strict access while deleting
      raise "bypassing strict access while deleting is not currently supported"
    end

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
        strict_access: true,
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
          bypass_strict_access?: params[:authorization][:bypass_strict_access?],
          verbose?: params[:verbose?]
        )
      else
        Engine.run([auth_request], api, fetch_only?: true, verbose?: params[:verbose?])
      end

    case result do
      %{errors: []} ->
        :ok

      %Ash.Engine{errors: errors} ->
        {:error, Ash.to_ash_error(errors)}
    end
  end
end
