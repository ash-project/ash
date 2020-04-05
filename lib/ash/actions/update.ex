defmodule Ash.Actions.Update do
  alias Ash.Engine
  alias Ash.Actions.{Attributes, Relationships, SideLoad}

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
         {:ok, %{data: updated}} = state <-
           do_authorized(changeset, params, action, resource, api, side_load_requests) do
      {:ok, SideLoad.attach_side_loads(updated, state)}
    else
      %Ecto.Changeset{} = changeset ->
        {:error, changeset}

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
      Ash.Engine2.Request.new(
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
          Ash.Engine2.Request.UnresolvedField.data([], fn request, _data ->
            resource
            |> Ash.DataLayer.update(request.changeset)
            |> case do
              {:ok, result} ->
                request.changeset
                |> Map.get(:__after_changes__, [])
                |> Enum.reduce_while({:ok, result}, fn func, {:ok, result} ->
                  case func.(request.changeset, result) do
                    {:ok, result} -> {:cont, {:ok, result}}
                    {:error, error} -> {:halt, {:error, error}}
                  end
                end)
            end
          end),
        path: :data,
        resolve_when_fetch_only?: true,
        name: "#{action.type} - `#{action.name}`"
      )

    attribute_requests = Attributes.attribute_change_requests(changeset, api, resource, action)

    relationship_requests = Map.get(changeset, :__requests__, [])

    if params[:authorization] do
      strict_access? =
        case Keyword.fetch(params[:authorization], :strict_access?) do
          {:ok, value} -> value
          :error -> false
        end

      Engine.run(
        params[:authorization][:user],
        [update_request | attribute_requests] ++ relationship_requests ++ side_load_requests,
        strict_access?: strict_access?,
        log_final_report?: params[:authorization][:log_final_report?] || false
      )
    else
      authorization = params[:authorization] || []

      Engine.run(
        authorization[:user],
        [update_request | attribute_requests] ++ relationship_requests ++ side_load_requests,
        fetch_only?: true
      )
    end
  end

  defp prepare_update_attributes(%resource{} = record, attributes) do
    allowed_keys =
      resource
      |> Ash.attributes()
      |> Enum.map(& &1.name)

    changeset =
      record
      |> Ecto.Changeset.cast(attributes, allowed_keys)
      |> Map.put(:action, :update)

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
end
