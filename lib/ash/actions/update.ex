defmodule Ash.Actions.Update do
  @moduledoc false
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Actions.{Relationships, SideLoad}
  require Logger

  @spec run(Ash.api(), Ash.record(), Ash.action(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  def run(api, %resource{} = record, action, opts) do
    attributes = Keyword.get(opts, :attributes, %{})
    relationships = Keyword.get(opts, :relationships, %{})
    side_load = opts[:side_load] || []

    engine_opts =
      opts
      |> Keyword.take([:verbose?, :actor, :authorize?])
      |> Keyword.put(:transaction?, true)

    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

    with {:ok, side_load_query} <- side_loads_as_query(api, resource, side_load),
         {:ok, relationships} <-
           Relationships.validate_not_changing_relationship_and_source_field(
             relationships,
             attributes,
             resource
           ),
         {:ok, attributes, relationships} <-
           Relationships.field_changes_into_relationship_changes(
             relationships,
             attributes,
             resource
           ),
         %{valid?: true} = changeset <- changeset(record, api, attributes, relationships),
         side_load_requests <-
           SideLoad.requests(side_load_query),
         %{data: %{commit: updated}, errors: []} = state <-
           do_run_requests(
             changeset,
             relationships,
             engine_opts,
             action,
             resource,
             api,
             side_load_requests
           ) do
      {:ok, SideLoad.attach_side_loads(updated, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, Ash.Error.Changeset.changeset_to_errors(resource, changeset)}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, error}
    end
  end

  def changeset(%resource{} = record, api, attributes, relationships) do
    record
    |> prepare_update_attributes(attributes)
    |> Relationships.handle_relationship_changes(api, relationships, :update)
    |> validate_constraints(resource)
  end

  defp validate_constraints(changeset, resource) do
    resource
    |> Ash.attributes()
    |> Enum.reduce(changeset, fn attribute, changeset ->
      with {:ok, value} <- Map.fetch(changeset.changes, attribute.name),
           {:error, error} <-
             Ash.Type.apply_constraints(attribute.type, value, attribute.constraints) do
        Ecto.Changeset.add_error(changeset, attribute.name, error)
      else
        _ ->
          changeset
      end
    end)
  end

  defp do_run_requests(
         changeset,
         relationships,
         engine_opts,
         action,
         resource,
         api,
         side_load_requests
       ) do
    authorization_request =
      Request.new(
        api: api,
        changeset:
          Relationships.changeset(
            changeset,
            api,
            relationships
          ),
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
              resource
              |> Ash.DataLayer.update(changeset)
              |> case do
                {:ok, result} ->
                  changeset
                  |> Map.get(:__after_changes__, [])
                  |> Enum.reduce_while({:ok, result}, fn func, {:ok, result} ->
                    case func.(changeset, result) do
                      {:ok, result} -> {:cont, {:ok, result}}
                      {:error, error} -> {:halt, {:error, error}}
                    end
                  end)
              end
            end
          ),
        path: [:commit],
        name: "#{action.type} - `#{action.name}` commit"
      )

    relationship_requests = Map.get(changeset, :__requests__, [])

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

  defp prepare_update_attributes(%resource{} = record, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.filter(& &1.writable?)
      |> Enum.map(& &1.name)

    {attributes, unwritable_attributes} =
      resource
      |> Ash.attributes()
      |> Enum.reduce({%{}, []}, fn attribute, {new_attributes, unwritable_attributes} ->
        provided_value = fetch_attr(attributes, attribute.name)
        provided? = match?({:ok, _}, provided_value)

        cond do
          provided? && !attribute.writable? ->
            {new_attributes, [attribute | unwritable_attributes]}

          provided? ->
            {:ok, value} = provided_value
            {Map.put(new_attributes, attribute.name, value), unwritable_attributes}

          is_nil(attribute.update_default) ->
            {new_attributes, unwritable_attributes}

          true ->
            {Map.put(new_attributes, attribute.name, update_default(attribute)),
             unwritable_attributes}
        end
      end)

    changeset =
      record
      |> Ecto.Changeset.cast(attributes, allowed_keys, empty_values: [])
      |> Map.put(:action, :update)

    changeset =
      Enum.reduce(
        unwritable_attributes,
        changeset,
        &Ecto.Changeset.add_error(&2, &1.name, "attribute is not writable")
      )

    resource
    |> Ash.attributes()
    |> Enum.reject(&Map.get(&1, :allow_nil?))
    |> Enum.reduce(changeset, fn attr, changeset ->
      case Ecto.Changeset.fetch_change(changeset, attr.name) do
        {:ok, nil} ->
          Ecto.Changeset.add_error(changeset, attr.name, "must not be nil")

        _ ->
          changeset
      end
    end)
  end

  defp fetch_attr(map, name) do
    case Map.fetch(map, name) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        Map.fetch(map, to_string(name))
    end
  end

  defp update_default(%{default: {:constant, value}}), do: value
  defp update_default(%{default: {mod, func, args}}), do: apply(mod, func, args)

  defp update_default(%{default: function}) when is_function(function, 0),
    do: function.()
end
