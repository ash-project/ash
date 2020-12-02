defmodule Ash.Actions.Destroy do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.api(), Ash.Changeset.t(), Ash.action(), Keyword.t()) ::
          :ok | {:error, Ash.Changeset.t()} | {:error, Ash.error()}
  def run(api, changeset, %{soft?: true} = action, opts) do
    case Ash.Actions.Update.run(api, %{changeset | action_type: :destroy}, action, opts) do
      {:ok, _} -> :ok
      other -> other
    end
  end

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

    changeset = %{changeset | action_type: :destroy, api: api}

    with %{valid?: true} <- Ash.Changeset.cast_arguments(changeset, action),
         :ok <- validate(changeset),
         :ok <- validate_multitenancy(changeset) do
      destroy_request =
        Request.new(
          resource: resource,
          api: api,
          path: [:destroy],
          action: action,
          authorize?: false,
          changeset: changeset,
          notify?: true,
          data:
            Request.resolve(
              [[:data, :data], [:destroy, :changeset]],
              fn %{destroy: %{changeset: changeset}} ->
                Ash.Changeset.with_hooks(changeset, fn changeset ->
                  case Ash.DataLayer.destroy(resource, changeset) do
                    :ok ->
                      {:ok, record}

                    {:error, error} ->
                      {:error, error}
                  end
                end)
              end
            )
        )

      case Engine.run([authorization_request, destroy_request], api, engine_opts) do
        %{errors: []} = engine_result ->
          add_notifications(engine_result, opts)

        {:error, errors} ->
          {:error, Ash.Error.to_ash_error(errors)}

        %{errors: errors} ->
          {:error, Ash.Error.to_ash_error(errors)}
      end
    else
      {:error, error} ->
        {:error, error}
    end
  end

  defp add_notifications(engine_result, opts) do
    if opts[:return_notifications?] do
      {:ok, Map.get(engine_result, :resource_notifications, [])}
    else
      :ok
    end
  end

  defp validate_multitenancy(changeset) do
    if Ash.Resource.multitenancy_strategy(changeset.resource) &&
         not Ash.Resource.multitenancy_global?(changeset.resource) && is_nil(changeset.tenant) do
      {:error, "#{inspect(changeset.resource)} changesets require a tenant to be specified"}
    else
      :ok
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
