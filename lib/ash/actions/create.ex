defmodule Ash.Actions.Create do
  @moduledoc false
  require Logger

  alias Ash.Actions.Helpers
  alias Ash.Engine
  alias Ash.Engine.Request

  @spec run(Ash.Api.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record()} | {:error, term}
  def run(api, changeset, action, opts) do
    upsert? = opts[:upsert?] || false
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
             api
           ) do
      add_notifications(created, engine_result, opts)
    else
      %Ash.Changeset{errors: errors} = changeset ->
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

      {:error, %Ash.Engine.Runner{errors: errors, changeset: changeset}} ->
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error, changeset: changeset)}
    end
  end

  defp add_tenant(data, changeset) do
    if Ash.Resource.Info.multitenancy_strategy(changeset.resource) do
      %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}
    else
      data
    end
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
      Ash.Changeset.for_create(changeset, action.name, %{}, actor: actor)
    end
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.cast_arguments(action)
  end

  defp do_run_requests(
         changeset,
         upsert?,
         engine_opts,
         action,
         resource,
         api
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
                      engine_opts[:actor]
                    )

                  {changeset, instructions}
                end)
                |> Ash.Changeset.with_hooks(fn changeset ->
                  changeset = Ash.Changeset.require_values(changeset, :create, true)

                  if changeset.valid? do
                    if upsert? do
                      Ash.DataLayer.upsert(resource, changeset)
                    else
                      Ash.DataLayer.create(resource, changeset)
                    end
                  else
                    {:error, changeset.errors}
                  end
                end)

              with {:ok, created, changeset, %{notifications: notifications}} <- result,
                   created <- add_tenant(created, changeset),
                   {:ok, loaded} <-
                     Ash.Actions.ManagedRelationships.load(api, created, changeset, engine_opts),
                   {:ok, with_relationships, new_notifications} <-
                     Ash.Actions.ManagedRelationships.manage_relationships(
                       loaded,
                       changeset,
                       engine_opts[:actor]
                     ) do
                {:ok, Helpers.select(with_relationships, changeset),
                 %{notifications: new_notifications ++ notifications}}
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
