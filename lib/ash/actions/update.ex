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

    changeset =
      if opts[:tenant] do
        Ash.Changeset.set_tenant(changeset, opts[:tenant])
      else
        changeset
      end

    resource = changeset.resource
    changeset = changeset(changeset, api, action, opts[:actor])

    with %{valid?: true} <- Ash.Changeset.validate_multitenancy(changeset),
         {:ok, %{data: %{commit: %^resource{} = updated}} = engine_result} <-
           do_run_requests(
             changeset,
             engine_opts,
             action,
             resource,
             api,
             opts
           ) do
      add_notifications(updated, changeset, engine_result, opts)
    else
      %Ash.Changeset{errors: errors} = changeset ->
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

      {:error, %Ash.Engine.Runner{errors: errors, changeset: runner_changeset}} ->
        errors = Helpers.process_errors(changeset, errors)
        {:error, Ash.Error.to_error_class(errors, changeset: runner_changeset || changeset)}

      {:error, error} ->
        error = Helpers.process_errors(changeset, error)
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

  defp run_after_action({:ok, result, instructions}, changeset, opts) do
    if opts[:after_action] do
      case opts[:after_action].(changeset, result) do
        {:ok, result} -> {:ok, Helpers.select(result, changeset), instructions}
        other -> other
      end
    else
      {:ok, Helpers.select(result, changeset), instructions}
    end
  end

  defp run_after_action(other, _, _), do: other

  defp add_notifications(result, changeset, engine_result, opts) do
    if opts[:return_notifications?] do
      if changeset.action_type == :destroy do
        {:ok, Map.get(engine_result, :resource_notifications, [])}
      else
        {:ok, result, Map.get(engine_result, :resource_notifications, [])}
      end
    else
      if changeset.action_type == :destroy do
        :ok
      else
        {:ok, result}
      end
    end
  end

  defp changeset(changeset, api, action, actor) do
    changeset = %{changeset | api: api}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_update(changeset, action.name, %{}, actor: actor)
    end
  end

  defp do_run_requests(
         changeset,
         engine_opts,
         action,
         resource,
         api,
         opts
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
                |> Ash.Changeset.before_action(
                  &Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                    &1,
                    engine_opts[:actor],
                    engine_opts
                  )
                )
                |> Ash.Changeset.with_hooks(fn changeset ->
                  case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                         changeset,
                         engine_opts[:actor],
                         engine_opts
                       ) do
                    {:error, error} ->
                      {:error, error}

                    {changeset, manage_instructions} ->
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
                          {:ok, changeset.data, %{notifications: []}}
                        else
                          if Ash.Changeset.changing_attributes?(changeset) do
                            changeset =
                              changeset
                              |> Ash.Changeset.set_defaults(:update, true)
                              |> Ash.Changeset.put_context(:changed?, false)

                            resource
                            |> Ash.DataLayer.update(changeset)
                            |> add_tenant(changeset)
                            |> manage_relationships(api, changeset, engine_opts)
                          else
                            changeset = Ash.Changeset.put_context(changeset, :changed?, false)

                            {:ok, changeset.data}
                            |> add_tenant(changeset)
                            |> manage_relationships(api, changeset, engine_opts)
                          end
                        end
                        |> case do
                          {:ok, result, notifications} ->
                            {:ok, result,
                             Map.update!(
                               notifications,
                               :notifications,
                               &(&1 ++ manage_instructions.notifications)
                             )}

                          {:error, error} ->
                            {:error, error}
                        end
                      else
                        {:error, changeset.errors}
                      end
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
                  |> run_after_action(changeset, opts)

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
      {:ok, with_relationships, %{notifications: new_notifications, new_changeset: changeset}}
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
