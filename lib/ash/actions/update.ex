defmodule Ash.Actions.Update do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine.Request

  require Logger

  @spec run(Ash.Api.t(), Ash.Resource.record(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, Ash.Changeset.t()}
          | {:error, term}
  def run(api, changeset, action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(api, changeset, opts)

    authorize? = authorize?(opts)
    return_notifications? = opts[:return_notifications?]
    actor = opts[:actor]
    verbose? = opts[:verbose?]
    after_action = opts[:after_action]
    resource = changeset.resource

    []
    |> as_requests(resource, api, action,
      changeset: changeset,
      authorize?: authorize?,
      actor: actor,
      timeout: opts[:timeout],
      after_action: after_action,
      tenant: opts[:tenant]
    )
    |> Ash.Engine.run(
      resource: resource,
      verbose?: verbose?,
      actor: actor,
      name: "#{inspect(resource)}.#{action.name}",
      notification_metadata: opts[:notification_metadata],
      return_notifications?: opts[:return_notifications?],
      authorize?: authorize?,
      timeout: opts[:timeout] || changeset.timeout || Ash.Api.Info.timeout(api),
      default_timeout: Ash.Api.Info.timeout(api),
      transaction?: Keyword.get(opts, :transaction?, true)
    )
    |> case do
      {:ok, %{data: %{commit: %^resource{} = updated}} = engine_result} ->
        add_notifications(
          resource,
          updated,
          action,
          changeset,
          engine_result,
          return_notifications?
        )

      {:error, %Ash.Engine{errors: errors, requests: requests}} ->
        case Enum.find_value(requests, fn request ->
               if request.path == [:commit] && match?(%Ash.Changeset{}, request.changeset) do
                 request.changeset
               end
             end) do
          nil ->
            errors = Helpers.process_errors(changeset, errors)
            {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

          changeset ->
            errors = Helpers.process_errors(changeset, errors)
            {:error, Ash.Error.to_error_class(errors, changeset: changeset)}
        end

      {:error, error} ->
        error = Helpers.process_errors(changeset, error)

        {:error, Ash.Error.to_error_class(error, changeset: changeset)}
    end
  end

  defp authorize?(opts) do
    if opts[:authorize?] == false do
      false
    else
      opts[:authorize?] || Keyword.has_key?(opts, :actor)
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

  defp run_after_action({:ok, result, instructions}, changeset, after_action) do
    if after_action do
      case after_action.(changeset, result) do
        {:ok, result} -> {:ok, Helpers.select(result, changeset), instructions}
        other -> other
      end
    else
      {:ok, Helpers.select(result, changeset), instructions}
    end
  end

  defp run_after_action(other, _, _), do: other

  defp add_notifications(
         resource,
         result,
         action,
         changeset,
         engine_result,
         return_notifications?
       ) do
    if return_notifications? do
      if changeset.action_type == :destroy do
        {:ok, Map.get(engine_result, :resource_notifications, [])}
      else
        {:ok, result, Map.get(engine_result, :resource_notifications, [])}
      end
    else
      Ash.Actions.Helpers.warn_missed!(resource, action, engine_result)

      if changeset.action_type == :destroy do
        :ok
      else
        {:ok, result}
      end
    end
  end

  defp changeset(changeset, api, action, opts) do
    changeset = %{changeset | api: api}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_update(changeset, action.name, %{}, opts)
    end
  end

  def as_requests(
        path,
        resource,
        api,
        action,
        request_opts
      ) do
    changeset_dependencies = request_opts[:changeset_dependencies]
    changeset = request_opts[:changeset]
    changeset_input = request_opts[:changeset_input] || fn _ -> %{} end
    modify_changeset = request_opts[:modify_changeset] || fn changeset, _ -> changeset end
    tenant = request_opts[:tenant]
    after_action = request_opts[:after_action]
    skip_on_nil_record? = request_opts[:skip_on_nil_record?]
    error_path = request_opts[:error_path]
    timeout = request_opts[:timeout]

    record =
      request_opts[:record] ||
        fn _ -> raise "`record` option must be passed if `changeset` is not" end

    authorization_request =
      Request.new(
        api: api,
        error_path: error_path,
        changeset:
          Request.resolve(changeset_dependencies, fn %{actor: actor, authorize?: authorize?} =
                                                       context ->
            input = changeset_input.(context) || %{}

            tenant =
              case tenant do
                nil ->
                  nil

                tenant when is_function(tenant) ->
                  tenant.(context)

                tenant ->
                  tenant
              end

            changeset =
              case changeset do
                nil ->
                  case record.(context) do
                    nil ->
                      if skip_on_nil_record? do
                        :skip
                      else
                        raise "record was nil but `skip_on_nil_record?` was not set!"
                      end

                    record ->
                      record
                      |> Ash.Changeset.for_update(action.name, input,
                        actor: actor,
                        tenant: tenant,
                        authorize?: authorize?,
                        timeout: timeout
                      )
                      |> changeset(api, action,
                        actor: actor,
                        tenant: tenant,
                        authorize?: authorize?,
                        timeout: timeout
                      )
                  end

                changeset ->
                  changeset(changeset, api, action,
                    actor: actor,
                    tenant: tenant,
                    authorize?: authorize?,
                    timeout: timeout
                  )
              end

            if changeset == :skip do
              {:ok, nil}
            else
              changeset = %{
                changeset
                | timeout: timeout || changeset.timeout || Ash.Api.Info.timeout(changeset.api)
              }

              changeset =
                if tenant do
                  Ash.Changeset.set_tenant(changeset, tenant)
                else
                  changeset
                end

              with %{valid?: true} = changeset <- modify_changeset.(changeset, context),
                   %{valid?: true} = changeset <- Ash.Changeset.validate_multitenancy(changeset) do
                {:ok, changeset}
              else
                %Ash.Changeset{valid?: false} = changeset ->
                  {:error, changeset.errors}
              end
            end
          end),
        action: action,
        resource: resource,
        data:
          Request.resolve([path ++ [:data, :changeset]], fn context ->
            case get_in(context, [path] ++ [:data, :changeset]) do
              nil ->
                {:ok, nil}

              changeset ->
                {:ok, changeset.data}
            end
          end),
        authorize?: true,
        path: path ++ [:data],
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    commit_request =
      Request.new(
        api: api,
        changeset:
          Request.resolve([path ++ [:data, :changeset]], fn context ->
            {:ok, get_in(context, path ++ [:data, :changeset])}
          end),
        action: action,
        resource: resource,
        notify?: true,
        error_path: error_path,
        authorize?: false,
        data:
          Request.resolve(
            [path ++ [:data, :changeset]],
            fn %{actor: actor, authorize?: authorize?} = context ->
              changeset = get_in(context, path ++ [:data, :changeset])

              if is_nil(changeset) && skip_on_nil_record? do
                {:ok, nil}
              else
                result =
                  changeset
                  |> Ash.Changeset.before_action(
                    &Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                      &1,
                      actor,
                      authorize?: authorize?,
                      actor: actor
                    )
                  )
                  |> Ash.Changeset.with_hooks(fn changeset ->
                    case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                           changeset,
                           actor,
                           actor: actor,
                           authorize?: authorize?
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
                                |> Ash.Changeset.put_context(:changed?, true)

                              resource
                              |> Ash.DataLayer.update(changeset)
                              |> add_tenant(changeset)
                              |> manage_relationships(api, changeset,
                                actor: actor,
                                authorize?: authorize?
                              )
                            else
                              changeset = Ash.Changeset.put_context(changeset, :changed?, false)

                              {:ok, changeset.data}
                              |> add_tenant(changeset)
                              |> manage_relationships(api, changeset,
                                actor: actor,
                                authorize?: authorize?
                              )
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
                      |> manage_relationships(api, changeset, actor: actor, authorize?: authorize?)
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
                    |> run_after_action(changeset, after_action)

                  other ->
                    other
                end
              end
            end
          ),
        path: path ++ [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    [authorization_request, commit_request]
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
