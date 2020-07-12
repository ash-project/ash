defmodule Ash.Actions.Create do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Actions.{Relationships, SideLoad}
  require Logger

  def run(api, changeset, action, opts) do
    side_load = opts[:side_load] || []
    upsert? = opts[:upsert?] || false
    resource = changeset.resource

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    with %{valid?: true} = changeset <-
           Relationships.handle_relationship_changes(%{changeset | api: api}),
         :ok <- check_upsert_support(changeset.resource, upsert?),
         {:ok, side_load_query} <-
           side_loads_as_query(changeset.api, changeset.resource, side_load),
         side_load_requests <-
           SideLoad.requests(side_load_query),
         %{
           data: %{commit: %^resource{} = created} = state,
           errors: []
         } <-
           do_run_requests(
             changeset,
             upsert?,
             engine_opts,
             action,
             resource,
             api,
             side_load_requests
           ) do
      {:ok, SideLoad.attach_side_loads(created, state)}
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
         upsert?,
         engine_opts,
         action,
         resource,
         api,
         side_load_requests
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
        data:
          Request.resolve(
            [[:commit, :changeset]],
            fn %{commit: %{changeset: changeset}} ->
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
      [authorization_request | [commit_request | relationship_read_requests]] ++
        side_load_requests,
      api,
      engine_opts
    )
  end

  defp check_upsert_support(_resource, false), do: :ok

  defp check_upsert_support(resource, true) do
    if Ash.data_layer_can?(resource, :upsert) do
      :ok
    else
      {:error, {:unsupported, :upsert}}
    end
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
