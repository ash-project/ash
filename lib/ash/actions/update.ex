defmodule Ash.Actions.Update do
  @moduledoc false
  alias Ash.Actions.Relationships
  alias Ash.Engine
  alias Ash.Engine.Request
  require Logger

  @spec run(Ash.api(), Ash.record(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.Changeset.t()} | {:error, Ash.error()}
  def run(api, changeset, action, opts) do
    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    resource = changeset.resource
    changeset = changeset(changeset, api, action, opts[:actor])

    with %{valid?: true} <- changeset,
         {:ok, %{data: %{commit: %^resource{} = updated}} = engine_result} <-
           do_run_requests(
             changeset,
             engine_opts,
             action,
             resource,
             api
           ) do
      updated
      |> add_tenant(changeset)
      |> add_notifications(engine_result, opts)
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
        Ash.Changeset.for_update(changeset, action.name, %{}, actor: actor)
      end

    Relationships.handle_relationship_changes(changeset)
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
        changeset: Relationships.changeset(changeset),
        action: action,
        resource: resource,
        data: changeset.data,
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
        data:
          Request.resolve(
            [[:data, :changeset]],
            fn %{data: %{changeset: changeset}} ->
              changeset
              |> Ash.Changeset.put_context(:actor, engine_opts[:actor])
              |> Ash.Changeset.with_hooks(fn changeset ->
                changeset = set_tenant(changeset)

                Ash.DataLayer.update(resource, changeset)
              end)
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    relationship_requests = changeset.requests

    Engine.run(
      [authorization_request | [commit_request | relationship_requests]],
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
end
