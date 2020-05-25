defmodule Ash.Actions.Update do
  alias Ash.Engine
  alias Ash.Actions.{Relationships, SideLoad}
  require Logger

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  # TODO: To support an upsert/ "find and update" pattern, we would support taking a filter instead of a record here.
  def run(api, %resource{} = record, action, params) do
    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

    transaction_result =
      Ash.DataLayer.transact(resource, fn ->
        do_run(api, record, action, params)
      end)

    case transaction_result do
      {:ok, value} -> value
      {:error, error} -> {:error, error}
    end
  end

  defp do_run(api, %resource{} = record, action, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    with {:ok, side_load_query} <- side_loads_as_query(api, resource, params[:side_load]),
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
         params <- Keyword.merge(params, attributes: attributes, relationships: relationships),
         %{valid?: true} = changeset <- changeset(record, api, params),
         side_load_requests <-
           SideLoad.requests(side_load_query, api.query(resource)),
         %{data: %{data: updated}, errors: []} = state <-
           do_run_requests(changeset, params, action, resource, api, side_load_requests) do
      {:ok, SideLoad.attach_side_loads(updated, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, Ash.Error.Changeset.changeset_to_errors(resource, changeset)}

      %Ash.Engine{errors: errors} ->
        {:error, Ash.to_ash_error(errors)}

      {:error, error} ->
        {:error, error}
    end
  end

  def changeset(record, api, params) do
    attributes = Keyword.get(params, :attributes, %{})
    relationships = Keyword.get(params, :relationships, %{})

    record
    |> prepare_update_attributes(attributes)
    |> Relationships.handle_relationship_changes(api, relationships, :update)
  end

  defp do_run_requests(changeset, params, action, resource, api, side_load_requests) do
    relationships = Keyword.get(params, :relationships)

    update_request =
      Ash.Engine.Request.new(
        api: api,
        changeset:
          Ash.Actions.Relationships.changeset(
            changeset,
            api,
            relationships
          ),
        action: Ash.primary_action!(resource, :read),
        resource: resource,
        data:
          Ash.Engine.Request.resolve(
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
        path: :data,
        name: "#{action.type} - `#{action.name}`"
      )

    relationship_requests = Map.get(changeset, :__requests__, [])

    Engine.run(
      [update_request | relationship_requests] ++ side_load_requests,
      api,
      verbose?: params[:verbose?]
    )
  end

  defp side_loads_as_query(_api, _resource, nil), do: {:ok, nil}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: []} = query), do: {:ok, query}
  defp side_loads_as_query(_api, _resource, %Ash.Query{errors: errors}), do: {:error, errors}

  defp side_loads_as_query(api, resource, side_loads) when is_list(side_loads) do
    resource
    |> api.query()
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
            {Map.put(new_attributes, attribute.name, update_default(attribute, record)),
             unwritable_attributes}
        end
      end)

    changeset =
      record
      |> Ecto.Changeset.cast(attributes, allowed_keys)
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

  defp update_default(%{default: {:constant, value}}, _record), do: value
  defp update_default(%{default: {mod, func}}, record), do: apply(mod, func, [record])

  defp update_default(%{default: function}, _record) when is_function(function, 0),
    do: function.()

  defp update_default(%{default: function}, record) when is_function(function, 1),
    do: function.(record)
end
