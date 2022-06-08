defmodule Ash.Actions.Create do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine.Request

  require Logger

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, term}
  def run(api, changeset, action, opts) do
    opts =
      case Map.fetch(changeset.context[:private] || %{}, :actor) do
        {:ok, actor} ->
          Keyword.put_new(opts, :actor, actor)

        _ ->
          opts
      end

    upsert? = opts[:upsert?] || get_in(changeset.context, [:private, :upsert?]) || false
    authorize? = authorize?(opts)
    upsert_keys = opts[:upsert_keys]

    upsert_identity =
      opts[:upsert_identity] || get_in(changeset.context, [:private, :upsert_identity])

    return_notifications? = opts[:return_notifications?]
    actor = opts[:actor]
    verbose? = opts[:verbose?]
    resource = changeset.resource

    []
    |> as_requests(resource, api, action,
      changeset: changeset,
      upsert?: upsert?,
      timeout: opts[:timeout],
      upsert_identity: upsert_identity,
      upsert_keys: upsert_keys,
      authorize?: authorize?,
      actor: actor,
      tenant: opts[:tenant],
      after_action: opts[:after_action]
    )
    |> Ash.Engine.run(
      resource: resource,
      verbose?: verbose?,
      actor: actor,
      authorize?: authorize?,
      notification_metadata: opts[:notification_metadata],
      timeout: opts[:timeout] || changeset.timeout || Ash.Api.timeout(api),
      transaction?: Keyword.get(opts, :transaction?, true)
    )
    |> case do
      {:ok, %{data: %{commit: %^resource{} = created}} = engine_result} ->
        add_notifications(action, created, engine_result, return_notifications?)

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

  defp add_tenant({:ok, nil}, _), do: {:ok, nil}

  defp add_tenant({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}}
    else
      {:ok, data}
    end
  end

  defp add_tenant(other, _), do: other

  defp add_notifications(_action, result, engine_result, true) do
    {:ok, result, Map.get(engine_result, :resource_notifications, [])}
  end

  defp add_notifications(action, result, engine_result, _) do
    Ash.Actions.Helpers.warn_missed!(action, engine_result)

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
    after_action = request_opts[:after_action]

    authorization_request =
      Request.new(
        api: api,
        resource: resource,
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
                  resource
                  |> Ash.Changeset.for_create(action.name, input,
                    actor: actor,
                    tenant: tenant,
                    timeout: timeout
                  )
                  |> changeset(api, action,
                    actor: actor,
                    tenant: tenant,
                    timeout: timeout
                  )

                changeset ->
                  changeset(changeset, api, action,
                    actor: actor,
                    tenant: tenant,
                    timeout: timeout
                  )
              end

            changeset = %{
              changeset
              | timeout: timeout || changeset.timeout || Ash.Api.timeout(api)
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
        authorize?: true,
        data: nil,
        path: path ++ [:data],
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    commit_request =
      Request.new(
        api: api,
        resource: resource,
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
            fn %{authorize?: authorize?, actor: actor} = data ->
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
                |> Ash.Changeset.put_context(:private, %{actor: actor})
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
                         authorize?: authorize?,
                         actor: actor
                       ) do
                    {:error, error} ->
                      {:error, error}

                    {changeset, manage_instructions} ->
                      changeset =
                        Ash.Changeset.require_values(
                          changeset,
                          :create,
                          true
                        )
                        |> Ash.Changeset.require_values(
                          :update,
                          false,
                          action.require_attributes
                        )

                      if changeset.valid? do
                        if action.manual? do
                          {:ok, nil}
                        else
                          if upsert? do
                            resource
                            |> Ash.DataLayer.upsert(changeset, upsert_keys)
                            |> add_tenant(changeset)
                            |> manage_relationships(api, changeset,
                              actor: actor,
                              authorize?: authorize?,
                              upsert?: upsert?
                            )
                          else
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
                        end
                      else
                        {:error, changeset.errors}
                      end
                  end
                end)

              case result do
                {:ok, nil, _changeset, _instructions} ->
                  if action.manual? do
                    {:error,
                     """
                     No record created in create action!
                     For manual actions, you must implement an `after_action` inside of a `change` that returns a newly created record.

                     For example:

                     # in the resource

                     action :special_create do
                       manual? true
                       change MyApp.DoCreate
                     end

                     # The change
                     defmodule MyApp.DoCreate do
                      use Ash.Resource.Change

                      def change(changeset, _, _) do
                        Ash.Changeset.after_action(changeset, fn changeset, _result ->
                          # result will be `nil`, because this is a manual action

                          result = do_something_that_creates_the_record(changeset)

                          {:ok, result}
                        end)
                      end
                     end
                     """}
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

                other ->
                  other
              end
            end
          ),
        path: path ++ [:commit],
        name: "#{action.type} - `#{action.name}`: commit"
      )

    [authorization_request, commit_request]
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

  defp manage_relationships({:ok, nil}, _, _, _) do
    {:ok, nil, %{notifications: []}}
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
