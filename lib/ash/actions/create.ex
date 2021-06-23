defmodule Ash.Actions.Create do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Engine
  alias Ash.Engine.Request

  require Logger

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, term}
  def run(api, changeset, action, opts) do
    upsert? = opts[:upsert?] || false

    upsert_keys =
      case opts[:upsert_identity] do
        nil ->
          Ash.Resource.Info.primary_key(changeset.resource)

        identity ->
          changeset.resource
          |> Ash.Resource.Info.identities()
          |> Enum.find(&(&1.name == identity))
          |> Kernel.||(
            raise ArgumentError,
                  "No identity found for #{inspect(changeset.resource)} called #{
                    inspect(identity)
                  }"
          )
          |> Map.get(:keys)
      end

    changeset =
      if opts[:tenant] do
        Ash.Changeset.set_tenant(changeset, opts[:tenant])
      else
        changeset
      end

    resource = changeset.resource

    opts =
      case Map.fetch(changeset.context[:private] || %{}, :actor) do
        {:ok, actor} ->
          Keyword.put_new(opts, :actor, actor)

        _ ->
          opts
      end

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

    changeset = changeset(changeset, api, action, opts[:actor])

    with %{valid?: true} <- Ash.Changeset.validate_multitenancy(changeset),
         :ok <- check_upsert_support(changeset.resource, upsert?),
         {:ok, %{data: %{commit: %^resource{} = created}} = engine_result} <-
           do_run_requests(
             changeset,
             upsert?,
             engine_opts,
             action,
             resource,
             api,
             upsert_keys
           ) do
      add_notifications(created, engine_result, opts)
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

  defp add_tenant({:ok, nil}, _), do: {:ok, nil}

  defp add_tenant({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}}
    else
      {:ok, data}
    end
  end

  defp add_tenant(other, _), do: other

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
      Ash.Changeset.for_create(changeset, action.name, %{}, actor: actor)
    end
    |> Ash.Changeset.set_defaults(:create, true)
  end

  defp do_run_requests(
         changeset,
         upsert?,
         engine_opts,
         action,
         resource,
         api,
         upsert_keys
       ) do
    authorization_request =
      Request.new(
        api: api,
        resource: resource,
        changeset: changeset,
        action: action,
        authorize?: false,
        data: nil,
        path: [:data],
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    commit_request =
      Request.new(
        api: api,
        resource: resource,
        changeset:
          Request.resolve([[:data, :changeset]], fn %{data: %{changeset: changeset}} ->
            {:ok, changeset}
          end),
        action: action,
        notify?: true,
        manage_changeset?: true,
        authorize?: false,
        data:
          Request.resolve(
            [[:commit, :changeset]],
            fn %{commit: %{changeset: changeset}} ->
              changeset = set_tenant(changeset)

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
                        |> manage_relationships(api, changeset, engine_opts)
                      else
                        resource
                        |> Ash.DataLayer.create(changeset)
                        |> add_tenant(changeset)
                        |> manage_relationships(api, changeset, engine_opts)
                      end
                    end
                  else
                    {:error, changeset.errors}
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

                {:ok, created, _changeset, instructions} ->
                  if action.manual? do
                    {:ok, created}
                    |> add_tenant(changeset)
                    |> manage_relationships(api, changeset, engine_opts)
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

                other ->
                  other
              end
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}`: commit"
      )

    Engine.run(
      [authorization_request, commit_request],
      api,
      engine_opts
    )
  end

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

  defp check_upsert_support(_resource, false), do: :ok

  defp check_upsert_support(resource, true) do
    if Ash.DataLayer.data_layer_can?(resource, :upsert) do
      :ok
    else
      {:error, {:unsupported, :upsert}}
    end
  end
end
