defmodule Ash.Actions.Destroy do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine.Request

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, list(Ash.Notifier.Notification.t())}
          | :ok
          | {:error, Ash.Changeset.t()}
          | {:error, term}
  def run(api, changeset, %{soft?: true} = action, opts) do
    changeset =
      if changeset.__validated_for_action__ == action.name do
        %{changeset | action_type: :destroy}
      else
        Ash.Changeset.for_destroy(%{changeset | action_type: :destroy}, action.name, %{},
          actor: opts[:actor]
        )
      end

    Ash.Actions.Update.run(api, changeset, action, opts)
  end

  def run(api, %{resource: resource} = changeset, action, opts) do
    opts =
      case Map.fetch(changeset.context[:private] || %{}, :actor) do
        {:ok, actor} ->
          Keyword.put_new(opts, :actor, actor)

        _ ->
          opts
      end

    authorize? = authorize?(opts)
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

    []
    |> as_requests(resource, api, action,
      changeset: changeset,
      authorize?: authorize?,
      actor: actor,
      timeout: opts[:timeout],
      tenant: opts[:tenant]
    )
    |> Ash.Engine.run(
      resource: resource,
      verbose?: verbose?,
      actor: actor,
      name: "#{inspect(resource)}.#{action.name}",
      return_notifications?: opts[:return_notifications?],
      notification_metadata: opts[:notification_metadata],
      authorize?: authorize?,
      timeout: opts[:timeout] || changeset.timeout || Ash.Api.timeout(api),
      transaction?: true
    )
    |> case do
      {:ok, %{data: data} = engine_result} ->
        resource
        |> add_notifications(action, engine_result, return_notifications?)
        |> add_destroyed(return_destroyed?, Map.get(data, :destroy))

      {:error, %Ash.Engine{errors: errors, requests: requests}} ->
        case Enum.find_value(requests, fn request ->
               if request.path == [:destroy] && match?(%Ash.Changeset{}, request.changeset) do
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
        changeset:
          Request.resolve(changeset_dependencies, fn %{actor: actor} = context ->
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
                        timeout: timeout
                      )
                      |> changeset(api, action,
                        actor: actor,
                        tenant: tenant,
                        timeout: timeout
                      )
                  end

                changeset ->
                  changeset(changeset, api, action,
                    actor: actor,
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
        name: "destroy request"
      )

    destroy_request =
      Request.new(
        resource: resource,
        api: api,
        path: path ++ [:destroy],
        action: action,
        authorize?: false,
        error_path: error_path,
        changeset:
          Request.resolve([path ++ [:data, :changeset]], fn context ->
            {:ok, get_in(context, path ++ [:data, :changeset])}
          end),
        notify?: true,
        authorize?: false,
        data:
          Request.resolve(
            [path ++ [:data, :data], path ++ [:destroy, :changeset]],
            fn %{actor: actor} = context ->
              changeset = get_in(context, path ++ [:destroy, :changeset])
              record = changeset.data

              changeset
              |> Ash.Changeset.put_context(:private, %{actor: actor})
              |> Ash.Changeset.with_hooks(fn changeset ->
                if action.manual? do
                  {:ok, record}
                else
                  case Ash.DataLayer.destroy(resource, changeset) do
                    :ok ->
                      {:ok,
                       Ash.Resource.Info.set_meta(record, %Ecto.Schema.Metadata{
                         state: :deleted,
                         schema: resource
                       })}

                    {:error, error} ->
                      {:error, error}
                  end
                end
              end)
              |> case do
                {:ok, result, changeset, instructions} ->
                  {:ok, Helpers.select(result, changeset), instructions}

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

  defp add_destroyed(:ok, true, destroyed) do
    {:ok, destroyed}
  end

  defp add_destroyed({:ok, notifications}, true, destroyed) do
    {:ok, destroyed, notifications}
  end

  defp add_destroyed(result, _, _) do
    result
  end

  defp authorize?(opts) do
    if opts[:authorize?] == false do
      false
    else
      opts[:authorize?] || Keyword.has_key?(opts, :actor)
    end
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
