defmodule Ash.Actions.Destroy do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine.Request

  require Ash.Tracer

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, list(Ash.Notifier.Notification.t())}
          | :ok
          | {:error, Ash.Changeset.t()}
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

      Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(api), :destroy], metadata do
        case do_run(api, changeset, action, opts) do
          {:error, error} ->
            if opts[:tracer] do
              stacktrace =
                case error do
                  %{stacktrace: %{stacktrace: stacktrace}} ->
                    stacktrace || []

                  _ ->
                    {:current_stacktrace, stacktrace} =
                      Process.info(self(), :current_stacktrace)

                    stacktrace
                end

              Ash.Tracer.set_handled_error(opts[:tracer], Ash.Error.to_error_class(error),
                stacktrace: stacktrace
              )
            end

            {:error, error}

          other ->
            other
        end
      end
    end
  end

  def do_run(api, changeset, %{soft?: true} = action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(api, changeset, opts)

    changeset =
      if changeset.__validated_for_action__ == action.name do
        %{changeset | action_type: :destroy}
      else
        Ash.Changeset.for_destroy(%{changeset | action_type: :destroy}, action.name, %{},
          actor: opts[:actor]
        )
      end

    Ash.Actions.Update.do_run(api, changeset, action, opts)
  end

  def do_run(api, %{resource: resource} = changeset, action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(api, changeset, opts)

    authorize? = opts[:authorize?]
    actor = opts[:actor]
    verbose? = opts[:verbose?]
    return_notifications? = opts[:return_notifications?]
    return_destroyed? = opts[:return_destroyed?]
    changeset = %{changeset | api: api}

    changeset =
      if opts[:tenant] do
        Ash.Changeset.set_tenant(changeset, opts[:tenant])
      else
        changeset
      end

    engine_timeout =
      if Keyword.get(opts, :transaction?, true) && action.transaction? do
        nil
      else
        opts[:timeout] || changeset.timeout || Ash.Api.Info.timeout(api)
      end

    []
    |> as_requests(resource, api, action,
      changeset: changeset,
      authorize?: authorize?,
      actor: actor,
      timeout: opts[:timeout] || changeset.timeout || Ash.Api.Info.timeout(api),
      return_notifications?: opts[:return_notifications?],
      return_destroyed?: return_destroyed?,
      tracer: opts[:tracer],
      timeout: opts[:timeout],
      tenant: opts[:tenant]
    )
    |> Ash.Engine.run(
      resource: resource,
      verbose?: verbose?,
      actor: actor,
      name: "#{inspect(resource)}.#{action.name}",
      tracer: opts[:tracer],
      return_notifications?: opts[:return_notifications?],
      notification_metadata: opts[:notification_metadata],
      authorize?: authorize?,
      timeout: engine_timeout,
      transaction?: false
    )
    |> case do
      {:ok, %{data: data} = engine_result} ->
        resource
        |> add_notifications(action, engine_result, return_notifications?)
        |> add_destroyed(return_destroyed?, data)

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

  def as_requests(path, resource, api, %{soft?: true} = action, request_opts) do
    Ash.Actions.Update.as_requests(path, resource, api, action, request_opts)
  end

  def as_requests(path, resource, api, action, request_opts) do
    changeset_dependencies = request_opts[:changeset_dependencies]
    changeset = request_opts[:changeset]
    changeset_input = request_opts[:changeset_input] || fn _ -> %{} end
    modify_changeset = request_opts[:modify_changeset] || fn changeset, _ -> changeset end
    tenant = request_opts[:tenant]
    skip_on_nil_record? = request_opts[:skip_on_nil_record?]
    error_path = request_opts[:error_path]
    timeout = request_opts[:timeout]
    default_timeout = request_opts[:default_timeout]
    tracer = request_opts[:tracer]
    authorize? = request_opts[:authorize?]
    return_destroyed? = request_opts[:return_destroyed?]

    record =
      request_opts[:record] ||
        fn _ -> raise "`record` option must be passed if `changeset` is not" end

    authorization_request =
      Request.new(
        resource: resource,
        api: api,
        path: path ++ [:data],
        action: action,
        error_path: error_path,
        async?: !(Keyword.get(request_opts, :transaction?, true) && action.transaction?),
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
                      |> Ash.Changeset.for_destroy(action.name, input,
                        actor: actor,
                        tenant: tenant,
                        authorize?: authorize?,
                        tracer: tracer,
                        timeout: timeout
                      )
                      |> changeset(api, action,
                        actor: actor,
                        tenant: tenant,
                        tracer: tracer,
                        authorize?: authorize?,
                        timeout: timeout
                      )
                  end

                changeset ->
                  changeset(changeset, api, action,
                    actor: actor,
                    authorize?: authorize?,
                    tracer: tracer,
                    tenant: tenant,
                    timeout: timeout
                  )
              end

            if changeset == :skip do
              {:ok, nil}
            else
              changeset = %{changeset | timeout: timeout || changeset.timeout || default_timeout}

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
        data:
          Request.resolve([path ++ [:data, :changeset]], fn context ->
            case get_in(context, [path] ++ [:data, :changeset]) do
              nil ->
                {:ok, nil}

              changeset ->
                {:ok, changeset.data}
            end
          end),
        name: "prepare #{inspect(resource)}.#{action.name}"
      )

    destroy_request =
      Request.new(
        resource: resource,
        api: api,
        path: path ++ [:commit],
        action: action,
        authorize?: false,
        error_path: error_path,
        async?: !(Keyword.get(request_opts, :transaction?, true) && action.transaction?),
        changeset:
          Request.resolve([path ++ [:data, :changeset]], fn context ->
            {:ok, get_in(context, path ++ [:data, :changeset])}
          end),
        notify?: true,
        authorize?: false,
        name: "commit #{inspect(resource)}.#{action.name}",
        data:
          Request.resolve(
            [path ++ [:data, :data], path ++ [:commit, :changeset]],
            fn %{actor: actor} = context ->
              changeset = get_in(context, path ++ [:commit, :changeset])
              record = changeset.data

              changeset
              |> Ash.Changeset.put_context(:private, %{actor: actor, authorize?: authorize?})
              |> Ash.Changeset.with_hooks(
                fn
                  %{valid?: false} = changeset ->
                    {:error, changeset}

                  changeset ->
                    if action.manual do
                      {mod, opts} = action.manual

                      if result = changeset.context[:private][:action_result] do
                        result
                      else
                        mod.destroy(changeset, opts, %{
                          actor: actor,
                          tenant: changeset.tenant,
                          authorize?: authorize?,
                          api: changeset.api
                        })
                      end
                    else
                      if result = changeset.context[:private][:action_result] do
                        result
                      else
                        case Ash.DataLayer.destroy(resource, changeset) do
                          :ok ->
                            {:ok,
                             Ash.Resource.set_meta(record, %Ecto.Schema.Metadata{
                               state: :deleted,
                               schema: resource
                             })}

                          {:error, error} ->
                            {:error, Ash.Changeset.add_error(changeset, error)}
                        end
                      end
                    end
                    |> then(fn result ->
                      if return_destroyed? do
                        Helpers.load(result, changeset, api,
                          actor: actor,
                          authorize?: authorize?,
                          tracer: tracer
                        )
                        |> Helpers.restrict_field_access(changeset)
                      else
                        result
                      end
                    end)
                end,
                transaction?:
                  Keyword.get(request_opts, :transaction?, true) && action.transaction?,
                timeout: request_opts[:timeout],
                return_notifications?: request_opts[:return_notifications?],
                transaction_metadata: %{
                  type: :destroy,
                  metadata: %{
                    actor: actor,
                    record: changeset.data,
                    resource: resource,
                    action: action.name
                  }
                }
              )
              |> case do
                {:ok, result, changeset, instructions} ->
                  instructions =
                    Map.update(
                      instructions,
                      :set_keys,
                      %{changeset: changeset, notification_data: result},
                      &Map.merge(&1, %{changeset: changeset, notification_data: result})
                    )

                  {:ok, Helpers.select(result, changeset), instructions}

                {:error, %Ash.Changeset{} = changeset} ->
                  {:error, changeset.errors, %{set: %{changeset: changeset}}}

                {:error, error} ->
                  {:error, error}
              end
            end
          )
      )

    [authorization_request, destroy_request]
  end

  defp add_notifications(resource, action, engine_result, return_notifications?) do
    if return_notifications? do
      {:ok, Map.get(engine_result, :resource_notifications, [])}
    else
      Ash.Actions.Helpers.warn_missed!(resource, action, engine_result)
      :ok
    end
  end

  defp add_destroyed(:ok, true, %{commit: destroyed}) do
    {:ok, destroyed}
  end

  defp add_destroyed({:ok, notifications}, true, %{commit: destroyed}) do
    {:ok, destroyed, notifications}
  end

  defp add_destroyed(result, _, _) do
    result
  end

  defp changeset(changeset, api, action, opts) do
    changeset = %{changeset | api: api}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_destroy(changeset, action.name, %{}, opts)
    end
  end
end
