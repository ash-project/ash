defmodule Ash.Actions.Destroy do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.api(), Ash.Changeset.t(), Ash.action(), Keyword.t()) ::
          :ok | {:error, Ash.Changeset.t()} | {:error, Ash.error()}
  def run(api, %{data: record, resource: resource} = changeset, action, opts) do
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
        changeset: %{changeset | action_type: :destroy, api: api},
        data:
          Request.resolve(
            [[:data, :data], [:destroy, :changeset]],
            fn %{destroy: %{changeset: changeset}} ->
              with :ok <- validate(changeset),
                   :ok <- Ash.DataLayer.destroy(resource, changeset) do
                {:ok, record}
              else
                {:error, error} -> {:error, error}
              end
            end
          )
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

  defp validate(changeset) do
    changeset.resource
    |> Ash.Resource.validations(:destroy)
    |> Enum.reduce(:ok, fn validation, acc ->
      if validation.expensive? and not changeset.valid? do
        acc
      else
        do_validation(changeset, validation, acc)
      end
    end)
  end

  defp do_validation(changeset, validation, acc) do
    case validation.module.validate(changeset, validation.opts) do
      :ok ->
        acc

      {:error, error} ->
        case acc do
          :ok -> {:error, [error]}
          {:error, errors} -> {:error, [error | errors]}
        end
    end
  end
end
