defmodule Ash.Actions.Create do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine.Request

  require Ash.Tracer
  require Logger

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, term}
  def run(api, changeset, action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(api, changeset, opts)

    Ash.Tracer.span :action,
                    Ash.Api.Info.span_name(
                      api,
                      changeset.resource,
                      action.name
                    ),
                    opts[:tracer] do
      metadata = %{
        api: api,
        resource: changeset.resource,
        resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

      Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(api), :create],
                                metadata do
        case do_run(api, changeset, action, opts) do
          {:error, error} ->
            if opts[:tracer] do
              opts[:tracer].set_error(Ash.Error.to_error_class(error))
            end

            {:error, error}

          other ->
            other
        end
      end
    end
  end

  defp do_run(api, changeset, action, opts) do
    upsert? =
      action.upsert? || opts[:upsert?] || get_in(changeset.context, [:private, :upsert?]) || false

    authorize? = opts[:authorize?]
    upsert_keys = opts[:upsert_keys]

    upsert_identity =
      if action.upsert? do
        action.upsert_identity || opts[:upsert_identity] ||
          get_in(changeset.context, [:private, :upsert_identity])
      else
        opts[:upsert_identity] || get_in(changeset.context, [:private, :upsert_identity])
      end

    changeset =
      Ash.Changeset.set_context(changeset, %{
        private: %{upsert?: true, upsert_identity: upsert_identity}
      })

    return_notifications? = opts[:return_notifications?]
    actor = opts[:actor]
    verbose? = opts[:verbose?]
    resource = changeset.resource

    engine_timeout =
      if Keyword.get(opts, :transaction?, true) && action.transaction? do
        nil
      else
        opts[:timeout] || changeset.timeout || Ash.Api.Info.timeout(api)
      end

    []
    |> as_requests(resource, api, action,
      changeset: changeset,
      upsert?: upsert?,
      timeout: opts[:timeout],
      upsert_identity: upsert_identity,
      upsert_keys: upsert_keys,
      return_notifications?: opts[:return_notifications?],
      authorize?: authorize?,
      actor: actor,
      tenant: opts[:tenant],
      tracer: opts[:tracer],
      after_action: opts[:after_action]
    )
    |> Ash.Engine.run(
      transaction?: false,
      resource: resource,
      verbose?: verbose?,
      name: "#{inspect(resource)}.#{action.name}",
      actor: actor,
      timeout: engine_timeout,
      tracer: opts[:tracer],
      authorize?: authorize?,
      notification_metadata: opts[:notification_metadata],
      return_notifications?: opts[:return_notifications?]
    )
    |> case do
      {:ok, %{data: %{commit: %^resource{} = created}} = engine_result} ->
        add_notifications(resource, action, created, engine_result, return_notifications?)

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

  defp add_tenant({:ok, nil}, _), do: {:ok, nil}

  defp add_tenant({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}}
    else
      {:ok, data}
    end
  end

  defp add_tenant(other, _), do: other

  defp add_notifications(_resource, _action, result, engine_result, true) do
    {:ok, result, Map.get(engine_result, :resource_notifications, [])}
  end

  defp add_notifications(resource, action, result, engine_result, _) do
    Ash.Actions.Helpers.warn_missed!(resource, action, engine_result)

    {:ok, result}
  end

  defp changeset(changeset, api, action, opts) do
    changeset = %{changeset | api: api}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_create(changeset, action.name, %{}, opts)
    end
    |> Ash.Changeset.set_defaults(:create, true)
  end

  def as_requests(path, resource, api, action, request_opts) do
    changeset_dependencies = request_opts[:changeset_dependencies] || []
    changeset = request_opts[:changeset]
    changeset_input = request_opts[:changeset_input] || fn _ -> %{} end
    modify_changeset = request_opts[:modify_changeset] || fn changeset, _ -> changeset end
    upsert? = request_opts[:upsert?]
    upsert_identity = request_opts[:upsert_identity]
    tenant = request_opts[:tenant]
    error_path = request_opts[:error_path]
    timeout = request_opts[:timeout]
    tracer = request_opts[:tracer]
    after_action = request_opts[:after_action]
    authorize? = request_opts[:authorize?]

    authorization_request =
      Request.new(
        api: api,
        resource: resource,
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
                  resource
                  |> Ash.Changeset.for_create(action.name, input,
                    actor: actor,
                    authorize?: authorize?,
                    tenant: tenant,
                    tracer: tracer,
                    timeout: timeout
                  )
                  |> changeset(api, action,
                    actor: actor,
                    authorize?: authorize?,
                    tracer: tracer,
                    tenant: tenant,
                    timeout: timeout
                  )

                changeset ->
                  changeset(changeset, api, action,
                    actor: actor,
                    authorize?: authorize?,
                    tracer: tracer,
                    tenant: tenant,
                    timeout: timeout
                  )
              end

            changeset = %{
              changeset
              | timeout: timeout || changeset.timeout || Ash.Api.Info.timeout(api)
            }

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
              if tenant do
                Ash.Changeset.set_tenant(changeset, tenant)
              else
                changeset
              end

            changeset = Ash.Changeset.set_defaults(changeset, :create, true)

            with %{valid?: true} = changeset <- modify_changeset.(changeset, context),
                 %{valid?: true} = changeset <- Ash.Changeset.validate_multitenancy(changeset),
                 :ok <- check_upsert_support(changeset.resource, upsert?) do
              {:ok, changeset}
            else
              %Ash.Changeset{valid?: false} = changeset ->
                {:error, changeset.errors}

              {:error, other} ->
                {:error, other}
            end
          end),
        action: action,
        async?: !(Keyword.get(request_opts, :transaction?, true) && action.transaction?),
        authorize?: true,
        data: nil,
        path: path ++ [:data],
        name: "prepare #{inspect(resource)}.#{action.name}"
      )

    commit_request =
      Request.new(
        api: api,
        resource: resource,
        async?: !(Keyword.get(request_opts, :transaction?, true) && action.transaction?),
        error_path: error_path,
        changeset:
          Request.resolve([path ++ [:data, :changeset]], fn data ->
            {:ok, get_in(data, path ++ [:data, :changeset])}
          end),
        action: action,
        notify?: true,
        authorize?: false,
        data:
          Request.resolve(
            [path ++ [:commit, :changeset]],
            fn %{actor: actor} = data ->
              changeset = get_in(data, path ++ [:commit, :changeset])

              upsert_keys =
                case upsert_identity do
                  nil ->
                    Ash.Resource.Info.primary_key(changeset.resource)

                  identity ->
                    changeset.resource
                    |> Ash.Resource.Info.identities()
                    |> Enum.find(&(&1.name == identity))
                    |> Kernel.||(
                      raise ArgumentError,
                            "No identity found for #{inspect(changeset.resource)} called #{inspect(identity)}"
                    )
                    |> Map.get(:keys)
                end

              changeset = set_tenant(changeset)

              result =
                changeset
                |> Ash.Changeset.with_hooks(
                  fn changeset ->
                    case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                           changeset,
                           actor,
                           authorize?: authorize?,
                           actor: actor
                         ) do
                      {:error, error} ->
                        {:error, error}

                      {changeset, manage_instructions} ->
                        changeset =
                          if changeset.context[:private][:action_result] do
                            changeset
                          else
                            Ash.Changeset.require_values(
                              changeset,
                              :create
                            )
                            |> Ash.Changeset.require_values(
                              :update,
                              false,
                              action.require_attributes
                            )
                          end

                        if changeset.valid? do
                          cond do
                            action.manual ->
                              {mod, opts} = action.manual

                              if result = changeset.context[:private][:action_result] do
                                result
                              else
                                mod.create(changeset, opts, %{
                                  actor: actor,
                                  tenant: changeset.tenant,
                                  authorize?: authorize?,
                                  api: changeset.api
                                })
                              end
                              |> add_tenant(changeset)
                              |> manage_relationships(api, changeset,
                                actor: actor,
                                authorize?: authorize?,
                                upsert?: upsert?
                              )

                            action.manual? ->
                              {:ok, nil}

                            true ->
                              belongs_to_attrs =
                                changeset.resource
                                |> Ash.Resource.Info.relationships()
                                |> Enum.filter(&(&1.type == :belongs_to))
                                |> Enum.map(& &1.source_attribute)

                              final_check =
                                changeset.resource
                                |> Ash.Resource.Info.attributes()
                                |> Enum.reject(
                                  &(&1.allow_nil? || &1.generated? || &1.name in belongs_to_attrs)
                                )

                              changeset =
                                if changeset.context[:private][:action_result] do
                                  changeset
                                else
                                  changeset =
                                    changeset
                                    |> Ash.Changeset.require_values(
                                      :create,
                                      true,
                                      final_check
                                    )

                                  {changeset, _} =
                                    Ash.Actions.ManagedRelationships.validate_required_belongs_to(
                                      {changeset, []},
                                      false
                                    )

                                  changeset
                                end

                              if changeset.valid? do
                                cond do
                                  result = changeset.context[:private][:action_result] ->
                                    result
                                    |> add_tenant(changeset)
                                    |> manage_relationships(api, changeset,
                                      actor: actor,
                                      authorize?: authorize?,
                                      upsert?: upsert?
                                    )

                                  upsert? ->
                                    resource
                                    |> Ash.DataLayer.upsert(changeset, upsert_keys)
                                    |> add_tenant(changeset)
                                    |> manage_relationships(api, changeset,
                                      actor: actor,
                                      authorize?: authorize?,
                                      upsert?: upsert?
                                    )

                                  true ->
                                    resource
                                    |> Ash.DataLayer.create(changeset)
                                    |> add_tenant(changeset)
                                    |> manage_relationships(api, changeset,
                                      actor: actor,
                                      authorize?: authorize?,
                                      upsert?: upsert?
                                    )
                                end
                                |> case do
                                  {:ok, result, instructions} ->
                                    {:ok, result,
                                     instructions
                                     |> Map.update!(
                                       :notifications,
                                       &(&1 ++ manage_instructions.notifications)
                                     )}

                                  {:error, error} ->
                                    {:error, Ash.Changeset.add_error(changeset, error)}
                                end
                              else
                                {:error, changeset}
                              end
                          end
                        else
                          {:error, changeset}
                        end
                    end
                  end,
                  transaction?:
                    Keyword.get(request_opts, :transaction?, true) && action.transaction?,
                  timeout: request_opts[:timeout],
                  tracer: request_opts[:tracer],
                  return_notifications?: request_opts[:return_notifications?],
                  transaction_metadata: %{
                    type: :create,
                    metadata: %{
                      resource: resource,
                      action: action.name,
                      actor: actor
                    }
                  }
                )

              case result do
                {:ok, nil, _changeset, _instructions} ->
                  if action.manual? do
                    {:error,
                     Ash.Error.Framework.ManualActionMissed.exception(
                       resource: resource,
                       action: action.name,
                       type: :create
                     )}
                  else
                    {:error, "No record created in create action!"}
                  end

                {:ok, created, changeset, instructions} ->
                  if action.manual? do
                    {:ok, created}
                    |> add_tenant(changeset)
                    |> manage_relationships(api, changeset, actor: actor, authorize?: authorize?)
                    |> case do
                      {:ok, result, %{notifications: new_notifications}} ->
                        {:ok, result,
                         Map.update!(instructions, :notifications, &(&1 ++ new_notifications))}

                      {:error, error} ->
                        {:error, error}
                    end
                  else
                    {:ok, created, instructions}
                  end
                  |> run_after_action(changeset,
                    after_action: after_action,
                    actor: actor,
                    authorize?: authorize?
                  )

                {:error, %Ash.Changeset{} = changeset} ->
                  {:error, changeset.errors, %{set: %{changeset: changeset}}}

                other ->
                  other
              end
            end
          ),
        path: path ++ [:commit],
        name: "perform #{inspect(resource)}.#{action.name}"
      )

    [authorization_request, commit_request]
  end

  defp run_after_action({:ok, result, instructions}, changeset, opts) do
    instructions =
      Map.update(
        instructions,
        :set_keys,
        %{changeset: changeset, notification_data: result},
        &Map.merge(&1, %{changeset: changeset, notification_data: result})
      )

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

  defp manage_relationships({:ok, nil}, _, _, _) do
    {:ok, nil, %{notifications: []}}
  end

  defp manage_relationships(
         {:ok, created, %{notifications: notifications}},
         api,
         changeset,
         engine_opts
       ) do
    case manage_relationships({:ok, created}, api, changeset, engine_opts) do
      {:ok, created, info} ->
        {:ok, created, Map.update(info, :notifications, notifications, &(&1 ++ notifications))}

      other ->
        other
    end
  end

  defp manage_relationships({:ok, created}, api, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(api, created, changeset, engine_opts),
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

  defp check_upsert_support(resource, true) do
    if Ash.DataLayer.data_layer_can?(resource, :upsert) do
      :ok
    else
      {:error, {:unsupported, :upsert}}
    end
  end

  defp check_upsert_support(_resource, _), do: :ok
end
