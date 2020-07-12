defmodule Ash.Actions.Update do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Actions.{Relationships, SideLoad}
  require Logger

  @spec run(Ash.api(), Ash.record(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, changeset, action, opts) do
    side_load = opts[:side_load] || []

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    resource = changeset.resource

    with %{valid?: true} = changeset <-
           Relationships.handle_relationship_changes(%{changeset | api: api}),
         {:ok, side_load_query} <-
           side_loads_as_query(changeset.api, changeset.resource, side_load),
         side_load_requests <-
           SideLoad.requests(side_load_query),
         %{data: %{commit: %^resource{} = updated}, errors: []} = state <-
           do_run_requests(
             changeset,
             engine_opts,
             action,
             resource,
             api,
             side_load_requests
           ) do
      {:ok, SideLoad.attach_side_loads(updated, state)}
    else
      %Ash.Changeset{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_run_requests(
         changeset,
         engine_opts,
         action,
         resource,
         api,
         side_load_requests
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
        data:
          Request.resolve(
            [[:data, :changeset]],
            fn %{data: %{changeset: changeset}} ->
              Ash.Changeset.with_hooks(changeset, fn changeset ->
                Ash.DataLayer.update(resource, changeset)
              end)
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    relationship_requests = changeset.requests

    Engine.run(
      [authorization_request | [commit_request | relationship_requests]] ++ side_load_requests,
      api,
      engine_opts
    )
  end

  defp side_loads_as_query(_api, _resource, nil), do: {:ok, nil}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: []} = query), do: {:ok, query}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: errors}), do: {:error, errors}

  defp side_loads_as_query(api, resource, side_loads) when is_list(side_loads) do
    resource
    |> Ash.Query.new(api)
    |> Ash.Query.side_load(side_loads)
    |> case do
      %{errors: []} = query -> {:ok, query}
      %{errors: errors} -> {:error, errors}
    end
  end
end
