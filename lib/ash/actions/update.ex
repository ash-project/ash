defmodule Ash.Actions.Update do
  alias Ash.Engine
  alias Ash.Actions.{Attributes, Relationships, SideLoad}
  require Logger

  @spec run(Ash.api(), Ash.record(), Ash.action(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ecto.Changeset.t()} | {:error, Ash.error()}
  # TODO: To support an upsert/ "find and update" pattern, we would support taking a filter instead of a record here.
  def run(api, %resource{} = record, action, params) do
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
    side_loads = Keyword.get(params, :side_load, [])
    side_load_filter = Keyword.get(params, :side_load_filter)

    with {:ok, relationships} <-
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
         {:ok, side_load_requests} <-
           SideLoad.requests(api, resource, side_loads, :update, side_load_filter),
         %{data: %{data: %{data: updated}}, errors: errors} = state
         when errors == %{} <-
           do_authorized(changeset, params, action, resource, api, side_load_requests) do
      {:ok, SideLoad.attach_side_loads(updated, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, changeset}

      %{errors: errors} ->
        {:error, errors}

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

  defp do_authorized(changeset, params, action, resource, api, side_load_requests) do
    relationships = Keyword.get(params, :relationships)

    update_request =
      Ash.Engine.Request.new(
        api: api,
        rules: action.rules,
        changeset:
          Ash.Actions.Relationships.changeset(
            changeset,
            api,
            relationships
          ),
        action_type: action.type,
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
        resolve_when_fetch_only?: true,
        name: "#{action.type} - `#{action.name}`"
      )

    attribute_requests = Attributes.attribute_change_requests(changeset, api, resource, action)

    relationship_requests = Map.get(changeset, :__requests__, [])

    if params[:authorization] do
      Engine.run(
        [update_request | attribute_requests] ++ relationship_requests ++ side_load_requests,
        api,
        strict_access?: false,
        user: params[:authorization][:user],
        bypass_strict_access?: params[:bypass_strict_access?],
        verbose?: params[:verbose?]
      )
    else
      Engine.run(
        [update_request | attribute_requests] ++ relationship_requests ++ side_load_requests,
        api,
        fetch_only?: true,
        verbose?: params[:verbose?]
      )
    end
  end

  defp prepare_update_attributes(%resource{} = record, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.filter(& &1.writeable?)
      |> Enum.map(& &1.name)

    {attributes, unwriteable_attributes} =
      resource
      |> Ash.attributes()
      |> Enum.reduce({%{}, []}, fn attribute, {new_attributes, unwriteable_attributes} ->
        cond do
          !attribute.writeable? && is_nil(attribute.default) ->
            {new_attributes, unwriteable_attributes}

          !attribute.writeable? ->
            {new_attributes, [attribute | unwriteable_attributes]}

          true ->
            case fetch_attr(attributes, attribute.name) do
              {:ok, value} ->
                {Map.put(new_attributes, attribute.name, value), unwriteable_attributes}

              :error ->
                {new_attributes, unwriteable_attributes}
            end
        end
      end)

    changeset =
      record
      |> Ecto.Changeset.cast(attributes, allowed_keys)
      |> Map.put(:action, :update)

    changeset =
      Enum.reduce(
        unwriteable_attributes,
        changeset,
        &Ecto.Changeset.add_error(&2, &1, "attribute is not writeable")
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
        value

      :error ->
        Map.fetch(map, to_string(name))
    end
  end
end
