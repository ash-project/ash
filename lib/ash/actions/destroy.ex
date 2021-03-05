defmodule Ash.Actions.Destroy do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          :ok | {:error, Ash.Changeset.t()} | {:error, term}
  def run(api, changeset, %{soft?: true} = action, opts) do
    changeset =
      Ash.Changeset.for_update(%{changeset | action_type: :destroy}, action.name, %{},
        actor: opts[:actor]
      )

    case Ash.Actions.Update.run(api, changeset, action, opts) do
      {:ok, _} -> :ok
      other -> other
    end
  end

  def run(api, %{data: record, resource: resource} = changeset, action, opts) do
    authorize? =
      if opts[:authorize?] == false do
        false
      else
        opts[:authorize?] || Keyword.has_key?(opts, :actor)
      end

    opts = Keyword.put(opts, :authorize?, authorize?)

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

    changeset =
      if changeset.__validated_for_action__ == action.name do
        changeset
      else
        Ash.Changeset.for_destroy(%{changeset | action_type: :destroy}, action.name, %{},
          actor: opts[:actor]
        )
      end

    changeset =
      changeset
      |> Ash.Changeset.cast_arguments(action)
      |> Ash.Changeset.validate_multitenancy()

    if changeset.valid? do
      destroy_request =
        Request.new(
          resource: resource,
          api: api,
          path: [:destroy],
          action: action,
          authorize?: false,
          changeset: changeset,
          notify?: true,
          manage_changeset?: true,
          authorize?: false,
          data:
            Request.resolve(
              [[:data, :data], [:destroy, :changeset]],
              fn %{destroy: %{changeset: changeset}} ->
                changeset
                |> Ash.Changeset.put_context(:private, %{actor: engine_opts[:actor]})
                |> Ash.Changeset.with_hooks(fn changeset ->
                  case Ash.DataLayer.destroy(resource, changeset) do
                    :ok ->
                      {:ok, record}

                    {:error, error} ->
                      {:error, error}
                  end
                end)
                |> case do
                  {:ok, result, _changeset, instructions} ->
                    {:ok, result, instructions}

                  {:error, error} ->
                    {:error, error}
                end
              end
            )
        )

      case Engine.run([authorization_request, destroy_request], api, engine_opts) do
        {:ok, engine_result} ->
          add_notifications(engine_result, opts)

        {:error, %Ash.Engine.Runner{errors: errors, changeset: changeset}} ->
          {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

        {:error, error} ->
          {:error, Ash.Error.to_error_class(error, changeset: changeset)}
      end
    else
      {:error, Ash.Error.to_error_class(changeset.errors, changeset: changeset)}
    end
  end

  defp add_notifications(engine_result, opts) do
    if opts[:return_notifications?] do
      {:ok, Map.get(engine_result, :resource_notifications, [])}
    else
      :ok
    end
  end
end
