defmodule Ash.Actions.Create do
  @moduledoc false
  alias Ash.Actions.Relationships
  alias Ash.Engine
  alias Ash.Engine.Request
  require Logger

  @spec run(Ash.api(), Ash.changeset(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def run(api, changeset, action, opts) do
    upsert? = opts[:upsert?] || false
    resource = changeset.resource

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    with %{valid?: true} = changeset <- changeset(changeset, api, action, opts[:actor]),
         :ok <- check_upsert_support(changeset.resource, upsert?),
         %{
           data: %{commit: %^resource{} = created},
           errors: []
         } = engine_result <-
           do_run_requests(
             changeset,
             upsert?,
             engine_opts,
             action,
             resource,
             api
           ) do
      created
      |> add_tenant(changeset)
      |> add_notifications(engine_result, opts)
    else
      %Ash.Changeset{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp add_tenant(data, changeset) do
    if Ash.Resource.multitenancy_strategy(changeset.resource) do
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

    changeset =
      if changeset.__validated_for_action__ == action.name do
        changeset
      else
        Ash.Changeset.for_create(changeset, action.name, %{}, actor: actor)
      end

    Relationships.handle_relationship_changes(changeset)
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
        changeset: Relationships.changeset(changeset),
        action: action,
        data: nil,
        path: [:data],
        name: "#{action.type} - `#{action.name}`: prepare"
      )

    relationship_read_requests = changeset.requests

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
        data:
          Request.resolve(
            [[:commit, :changeset]],
            fn %{commit: %{changeset: changeset}} ->
              changeset = set_tenant(changeset)

              Ash.Changeset.with_hooks(changeset, fn changeset ->
                if upsert? do
                  Ash.DataLayer.upsert(resource, changeset)
                else
                  Ash.DataLayer.create(resource, changeset)
                end
              end)
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}`: commit"
      )

    Engine.run(
      [authorization_request | [commit_request | relationship_read_requests]],
      api,
      engine_opts
    )
  end

  defp set_tenant(changeset) do
    if changeset.tenant && Ash.Resource.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.tenant | a])

      changeset
      |> Ash.Changeset.force_change_attribute(attribute, attribute_value)
      |> Map.put(:tenant, nil)
    else
      changeset
    end
  end

  defp check_upsert_support(_resource, false), do: :ok

  defp check_upsert_support(resource, true) do
    if Ash.Resource.data_layer_can?(resource, :upsert) do
      :ok
    else
      {:error, {:unsupported, :upsert}}
    end
  end
end
