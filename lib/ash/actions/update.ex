defmodule Ash.Actions.Update do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine
  alias Ash.Engine.Request

  require Logger

  @spec run(Ash.Api.t(), Ash.Resource.record(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, Ash.Changeset.t()}
          | {:error, term}
  def run(api, changeset, action, opts) do
    authorize? =
      if opts[:authorize?] == false do
        false
      else
        opts[:authorize?] || Keyword.has_key?(opts, :actor)
      end

    opts = Keyword.put(opts, :authorize?, authorize?)

    opts =
      case Map.fetch(changeset.context[:private] || %{}, :actor) do
        {:ok, actor} ->
          Keyword.put_new(opts, :actor, actor)

        _ ->
          opts
      end

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    resource = changeset.resource
    changeset = changeset(changeset, api, action, opts[:actor])

    with %{valid?: true} <- Ash.Changeset.validate_multitenancy(changeset),
         {:ok, %{data: %{commit: %^resource{} = updated}} = engine_result} <-
           do_run_requests(
             changeset,
             engine_opts,
             action,
             resource,
             api
           ) do
      add_notifications(updated, engine_result, opts)
    else
      %Ash.Changeset{errors: errors} = changeset ->
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

      {:error, %Ash.Engine.Runner{errors: errors, changeset: runner_changeset}} ->
        {:error, Ash.Error.to_error_class(errors, changeset: runner_changeset || changeset)}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error, changeset: changeset)}
    end
  end

  defp add_tenant({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}}
    else
      {:ok, data}
    end
  end

  defp add_tenant(other, _changeset) do
    other
  end

  defp add_notifications(result, engine_result, opts) do
    if opts[:return_notifications?] do
      {:ok, result, Map.get(engine_result, :resource_notifications, [])}
    else
      {:ok, result}
    end
  end

  defp changeset(changeset, api, action, actor) do
    changeset = %{changeset | api: api}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_update(changeset, action.name, %{}, actor: actor)
    end
    |> Ash.Changeset.set_defaults(:update, true)
  end

  defp do_run_requests(
         changeset,
         engine_opts,
         action,
         resource,
         api
       ) do
    authorization_request =
      Request.new(
        api: api,
        changeset: changeset,
        action: action,
        resource: resource,
        data: changeset.data,
        authorize?: false,
        path: :data,
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    commit_request =
      Request.new(
        api: api,
        changeset:
          Request.resolve([[:data, :changeset]], fn %{data: %{changeset: changeset}} ->
            {:ok, changeset}
          end),
        action: action,
        resource: resource,
        notify?: true,
        manage_changeset?: true,
        authorize?: false,
        data:
          Request.resolve(
            [[:data, :changeset]],
            fn %{data: %{changeset: changeset}} ->
              result =
                changeset
                |> Ash.Changeset.put_context(:private, %{actor: engine_opts[:actor]})
                |> Ash.Changeset.before_action(fn changeset ->
                  {changeset, instructions} =
                    Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                      changeset,
                      engine_opts[:actor],
                      engine_opts
                    )

                  {changeset, instructions}
                end)
                |> Ash.Changeset.with_hooks(fn changeset ->
                  changeset =
                    changeset
                    |> Ash.Changeset.require_values(
                      :update,
                      true
                    )
                    |> Ash.Changeset.require_values(
                      :update,
                      false,
                      action.require_attributes
                    )

                  changeset = set_tenant(changeset)

                  if changeset.valid? do
                    if action.manual? do
                      {:ok, changeset.data}
                    else
                      resource
                      |> Ash.DataLayer.update(changeset)
                      |> add_tenant(changeset)
                      |> manage_relationships(api, changeset, engine_opts)
                    end
                  else
                    {:error, changeset.errors}
                  end
                end)

              case result do
                {:ok, updated, changeset, instructions} ->
                  if action.manual? do
                    updated = updated || changeset.data

                    {:ok, updated}
                    |> add_tenant(changeset)
                    |> manage_relationships(api, changeset, engine_opts)
                    |> case do
                      {:ok, data, %{notifications: new_notifications}} ->
                        {:ok, data,
                         Map.update!(instructions, :notifications, &(&1 ++ new_notifications))}

                      {:error, error} ->
                        {:error, error}
                    end
                  else
                    {:ok, updated, instructions}
                  end

                other ->
                  other
              end
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    Engine.run(
      [authorization_request, commit_request],
      api,
      engine_opts
    )
  end

  defp manage_relationships({:ok, updated}, api, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(api, updated, changeset, engine_opts),
         {:ok, with_relationships, new_notifications} <-
           Ash.Actions.ManagedRelationships.manage_relationships(
             loaded,
             changeset,
             engine_opts[:actor],
             engine_opts
           ) do
      {:ok, Helpers.select(with_relationships, changeset), %{notifications: new_notifications}}
    end
  end

  defp manage_relationships(other, _, _, _), do: other

  defp set_tenant(changeset) do
    if changeset.tenant &&
         Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.Info.multitenancy_attribute(changeset.resource)

      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.tenant | a])

      Ash.Changeset.force_change_attribute(changeset, attribute, attribute_value)
    else
      changeset
    end
  end
end
